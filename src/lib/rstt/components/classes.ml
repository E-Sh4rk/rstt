open Sstt

type 'r tail =
| NoOther | AllOthers | Unknown
| RowVars of ('r list * 'r list) list
type 'r atom = { pos:attrs ; neg:attrs ; unk:attrs ; tail:'r tail }
and attrs = line list
and line = L of string * attrs

module LabelSet = Set.Make(Label)

type label_info = { sub: LabelSet.t; trans: LabelSet.t }
let labels = Hashtbl.create 10
let infos = Hashtbl.create 10
let top_classes = ref LabelSet.empty
let tag = Tag.mk "class"

let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

let new_class ~name ~subclass =
  let trans_of lbls =
    let trans = LabelSet.to_list lbls |>
                List.map (fun lbl -> (Hashtbl.find infos lbl).trans) in
    List.fold_left LabelSet.union lbls trans
  in
  try
    let n = Label.mk name in
    let sub = subclass |> List.map (Hashtbl.find labels) |> LabelSet.of_list in
    let trans = trans_of sub in
    let sub = sub |> LabelSet.filter (fun n ->
        LabelSet.subset trans (LabelSet.remove n sub |> trans_of) |> not) in
    let trans = LabelSet.add n trans in
    Hashtbl.add infos n { sub ; trans } ; Hashtbl.add labels name n ;
    sub |> LabelSet.iter (fun n -> top_classes := LabelSet.remove n !top_classes) ;
    top_classes := LabelSet.add n !top_classes ;
    n
  with Not_found -> invalid_arg "undefined sub-attributes"


let define_class name ~subclass =
  if Hashtbl.mem labels name then invalid_arg "Class already defined" ;
  new_class ~name ~subclass |> ignore

let is_defined name = Hashtbl.mem labels name

let map_tail f t =
  match t with
  | NoOther -> NoOther | AllOthers -> AllOthers | Unknown -> Unknown
  | RowVars lst -> RowVars (List.map (fun (ps,ns) -> List.map f ps, List.map f ns) lst)
let map_atom f ({pos;neg;unk;tail}:'a atom) : 'b atom = {pos;neg;unk;tail=map_tail f tail}

let rec labels_of_attrs lst =
  lst |> List.map (fun (L (str,l)) ->
      if is_defined str |> not then define_class str ~subclass:[] ;
      let lbl = Hashtbl.find labels str in
      let all = (Hashtbl.find infos lbl).trans in
      LabelSet.diff all (labels_of_attrs l)
    ) |> List.fold_left LabelSet.union LabelSet.empty

let tt, ff = Enum.mk "tt", Enum.mk "ff"
let tt, ff = Descr.mk_enum tt |> Ty.mk_descr, Descr.mk_enum ff |> Ty.mk_descr
let bb = Ty.cup tt ff
let mk {pos;neg;unk;tail} =
  let pos, unk, neg = labels_of_attrs pos, labels_of_attrs unk, labels_of_attrs neg in
  let pbindings = pos |> LabelSet.to_list |> List.map (fun lbl -> lbl, Ty.F.mk_descr (Ty.O.required tt)) in
  let ubindings = unk |> LabelSet.to_list |> List.map (fun lbl -> lbl, Ty.F.mk_descr (Ty.O.required bb)) in
  let nbindings = neg |> LabelSet.to_list |> List.map (fun lbl -> lbl, Ty.F.mk_descr (Ty.O.required ff)) in
  let bindings = LabelMap.of_list (pbindings@nbindings@ubindings) in
  let tail = match tail with
  | RowVars dnf -> dnf |> List.map (fun (ps,ns) -> (ps,ns,Ty.O.required bb)) |> Ty.F.of_dnf
  | NoOther -> ff |> Ty.O.required |> Ty.F.mk_descr
  | AllOthers -> tt |> Ty.O.required |> Ty.F.mk_descr
  | Unknown -> bb |> Ty.O.required |> Ty.F.mk_descr
  in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag

let any = mk {pos=[];neg=[];unk=[];tail=Unknown}
let any_d = proj_tag any
let noclass = mk {pos=[];neg=[];unk=[];tail=NoOther}

let extract_record ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid attr encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Op.Records'.approx
let record_to_atom r =
  let open Op.Records'.Atom in
  let bindings = r.bindings |> LabelMap.to_list in
  let ty_of_field fty =
    let dnf = Ty.F.dnf fty in
    let aux (ps,ns,leaf) =
      match ps, ns with
      | [], [] -> Ty.O.get leaf
      | _, _ -> Ty.empty
      in
    List.map aux dnf |> Ty.disj
  in
  let is_tt fty = let ty = ty_of_field fty in Ty.leq ty tt in
  let is_ff fty = let ty = ty_of_field fty in Ty.leq ty ff in
  let pos, bindings = bindings |> List.partition_map (fun (lbl, fty) ->
    if is_tt fty then Either.left lbl else Either.right (lbl,fty)
    ) in
  let neg, bindings = bindings |> List.partition_map (fun (lbl, fty) ->
    if is_ff fty then Either.left lbl else Either.right (lbl, fty)
    ) in
  let unk = bindings |> List.map fst in
  let pos, neg, unk = LabelSet.of_list pos, LabelSet.of_list neg, LabelSet.of_list unk in
  let rec aux labels pos lbl =
    let info = Hashtbl.find infos lbl in
    let sub = LabelSet.to_list info.sub in
    if (LabelSet.mem lbl labels) = pos
    then [L (Label.name lbl, List.concat_map (aux labels (not pos)) sub)]
    else List.concat_map (aux labels pos) sub
  in
  let pos = !top_classes |> LabelSet.to_list |> List.concat_map (aux pos true) in
  let neg = !top_classes |> LabelSet.to_list |> List.concat_map (aux neg true) in
  let unk = !top_classes |> LabelSet.to_list |> List.concat_map (aux unk true) in
  let tail =
    match is_tt r.tail, is_ff r.tail with
    | true, true -> RowVars (r.tail |> Ty.F.dnf |> List.map (fun (ps,ns,_) -> ps,ns))
    | true, false -> AllOthers
    | false, true -> NoOther
    | false, false -> Unknown
  in
  {pos ; neg ; unk ; tail}

let extract t : RowVar.t atom =
  extract_record t |> record_to_atom
let to_t _ comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty)
  else None

let destruct ty = proj_tag ty |> extract

let print _prec _assoc fmt {pos;neg;unk;tail} =
  let open Sstt.Prec in
  let print_tuple p fmt t =
    match t with
    | [] -> ()
    | [e] -> p fmt e
    | lst -> Format.fprintf fmt "(%a)" (Rstt_utils.print_seq p ", ") lst
  in
  let rec print_pos_line fmt (L (str, t)) =
    if t = []
    then
      Format.fprintf fmt "%s" str
    else
      Format.fprintf fmt "%s \ %a"
        str (print_tuple print_pos_line) t
  in
  let print_neg_line fmt (L (str, t)) =
    if t = []
    then
      Format.fprintf fmt "~%s" str
    else
      Format.fprintf fmt "~(%s \ %a)"
        str (print_tuple print_pos_line) t
  in
  let print_unk_line fmt (L (str, t)) =
    if t = []
    then
      Format.fprintf fmt "?%s" str
    else
      Format.fprintf fmt "?(%s \ %a)"
        str (print_tuple print_pos_line) t
  in
  let print_line fmt (kind,t) =
    match kind with
    | `Pos -> print_pos_line fmt t
    | `Neg -> print_neg_line fmt t
    | `Unk -> print_unk_line fmt t
  in
  let print fmt t = print_tuple print_line fmt t in
  let print_rv _prec _assoc fmt rv = RowVar.pp fmt rv in
  let print_tail fmt t =
    match t with
    | NoOther -> ()
    | AllOthers -> Format.fprintf fmt " *"
    | Unknown -> Format.fprintf fmt " ?"
    | RowVars dnf ->
      Format.fprintf fmt " ; %a"
        (Prec.print_non_empty_dnf ~any:"any" print_rv Prec.min_prec NoAssoc) dnf
  in
  let bindings =
    (pos |> List.map (fun p -> `Pos,p))@
    (neg |> List.map (fun n -> `Neg,n))@
    (unk |> List.map (fun n -> `Unk,n))
  in
  Format.fprintf fmt "<%a%a>" print bindings print_tail tail

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun _ x -> x) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
