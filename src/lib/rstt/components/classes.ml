open Sstt

type 'r tail =
| Closed | Open
| RowVars of ('r list * 'r list) list
type 'r atom = attrs * attrs * 'r tail
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


let define_class ~name ~subclass =
  if Hashtbl.mem labels name then invalid_arg "Class already defined" ;
  new_class ~name ~subclass |> ignore

let map_tail f t =
  match t with
  | Closed -> Closed | Open -> Open
  | RowVars lst -> RowVars (List.map (fun (ps,ns) -> List.map f ps, List.map f ns) lst)
let map_atom f ((a1,a2,tail):'a atom) : 'b atom = (a1,a2,map_tail f tail)

let rec labels_of_attrs lst =
  lst |> List.map (fun (L (str,l)) ->
    let lbl = Hashtbl.find labels str in
    let all = (Hashtbl.find infos lbl).trans in
    LabelSet.diff all (labels_of_attrs l)
    ) |> List.fold_left LabelSet.union LabelSet.empty

let tt, ff = Enum.mk "tt", Enum.mk "ff"
let tt, ff = Descr.mk_enum tt |> Ty.mk_descr, Descr.mk_enum ff |> Ty.mk_descr
let bb = Ty.cup tt ff
let mk (pos,neg,tail) =
  let pos, neg = labels_of_attrs pos, labels_of_attrs neg in
  let pbindings = pos |> LabelSet.to_list |> List.map (fun lbl -> lbl, Ty.F.mk_descr (Ty.O.required tt)) in
  let nbindings = neg |> LabelSet.to_list |> List.map (fun lbl -> lbl, Ty.F.mk_descr (Ty.O.required ff)) in
  let bindings = LabelMap.of_list (pbindings@nbindings) in
  let tail = match tail with
  | RowVars dnf -> dnf |> List.map (fun (ps,ns) -> (ps,ns,Ty.O.required bb)) |> Ty.F.of_dnf
  | Closed -> ff |> Ty.O.required |> Ty.F.mk_descr
  | Open -> tt |> Ty.O.required |> Ty.F.mk_descr
  in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag

let any = mk ([],[],RowVars [[],[]]) |> proj_tag
let any_d = proj_tag any

let extract_record ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid attr encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Op.Records'.approx
let record_to_atom r =
  let open Op.Records'.Atom in
  let bindings = r.bindings |> LabelMap.to_list in
  let is_tt fty = let ty = Ty.F.get_descr fty |> Ty.O.get in Ty.leq ty tt in
  let is_ff fty = let ty = Ty.F.get_descr fty |> Ty.O.get in Ty.leq ty ff in
  let pos = bindings |> List.filter_map (fun (lbl, fty) ->
    if is_tt fty then Some lbl else None
    ) |> LabelSet.of_list in
  let neg = bindings |> List.filter_map (fun (lbl, fty) ->
    if is_ff fty then Some lbl else None
    ) |> LabelSet.of_list in
  let rec aux labels pos lbl =
    let info = Hashtbl.find infos lbl in
    let sub = LabelSet.to_list info.sub in
    if (LabelSet.mem lbl labels) = pos
    then [L (Label.name lbl, List.concat_map (aux labels (not pos)) sub)]
    else List.concat_map (aux labels pos) sub
  in
  let pos = !top_classes |> LabelSet.to_list |> List.concat_map (aux pos true) in
  let neg = !top_classes |> LabelSet.to_list |> List.concat_map (aux neg true) in
  let tail =
    if is_tt r.tail then Open
    else if is_ff r.tail then Closed
    else RowVars (r.tail |> Ty.F.dnf |> List.map (fun (ps,ns,_) -> ps,ns))
  in
  (pos, neg, tail)

let extract t : RowVar.t atom =
  extract_record t |> record_to_atom
let to_t _ comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty)
  else None

let destruct ty = proj_tag ty |> extract

let print _prec _assoc _fmt _t = failwith "TODO"
  (* let rec print_line prec assoc fmt (L (n, t)) =
    let sym,prec',_ as opinfo = binop_info Diff in
    if t = []
    then
      Format.fprintf fmt "%s" (Node.name n)
    else
      fprintf prec assoc opinfo fmt "%s%s%a"
        (Node.name n) sym
        (print_cup print_line prec' NoAssoc) t
  in
  print_cup print_line prec assoc fmt t *)

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun _ x -> x) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
