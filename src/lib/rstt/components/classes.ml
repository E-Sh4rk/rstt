open Sstt

type 'r tail =
| NoOther | AllOthers | Unknown
| RowVars of ('r list * 'r list) list
type 'r atom = { pos:attrs ; neg:attrs ; unk:attrs ; tail:'r tail }
and attrs = line list
and line = L of string * attrs

module LabelSet = Set.Make(Label)

type label_info = { name:string; sub: LabelSet.t; trans: LabelSet.t }
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
    let n = Label.mk (Utils.slugify name) in
    let sub = subclass |> List.map (Hashtbl.find labels) |> LabelSet.of_list in
    let trans = trans_of sub in
    let sub = sub |> LabelSet.filter (fun n ->
        LabelSet.subset trans (LabelSet.remove n sub |> trans_of) |> not) in
    let trans = LabelSet.add n trans in
    Hashtbl.add infos n { name ; sub ; trans } ; Hashtbl.add labels name n ;
    sub |> LabelSet.iter (fun n -> top_classes := LabelSet.remove n !top_classes) ;
    top_classes := LabelSet.add n !top_classes ;
    n
  with Not_found -> invalid_arg "undefined sub-attributes"


let define_class name ~subclass =
  if Hashtbl.mem labels name then invalid_arg "Class already defined" ;
  new_class ~name ~subclass |> ignore

let is_defined name = Hashtbl.mem labels name

(* Method classes are written [@m], or [@"m"] when [m] is not an identifier *)
let class_of_method m =
  let is_start c = match c with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false in
  let is_char c = is_start c || match c with '0'..'9' | '\'' | '.' -> true | _ -> false in
  let is_ident m =
    let n = String.length m in
    let from i = i < n && is_start m.[i] && String.for_all is_char (String.sub m i (n-i)) in
    from 0 || (n > 0 && m.[0] = '.' && from 1)
  in
  if is_ident m then "@"^m else Printf.sprintf "@%S" m

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
(* A list of lines denotes the union of their labels: a class shared by several
   parts of the hierarchy may be reached several times, so we remove the lines
   that are already covered by the others. *)
let remove_redundant lines =
  let union ds = List.fold_left LabelSet.union LabelSet.empty ds in
  let rec aux kept lines =
    match lines with
    | [] -> List.rev kept
    | (l,d)::lines ->
      let others = union (List.map snd kept @ List.map snd lines) in
      if LabelSet.subset d others then aux kept lines else aux ((l,d)::kept) lines
  in
  lines |> List.map (fun l -> l, labels_of_attrs [l]) |> aux [] |> List.map fst

let record_to_atom r =
  let open Op.Records'.Atom in
  let bindings = r.bindings |> LabelMap.to_list in
  let ty_of_field fty =
    let dnf = Ty.F.dnf fty in
    let aux (ps,ns,leaf) =
      match ps, ns with
      | [], [] -> Ty.O.get leaf |> Ty.O.Atom.get
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
    then [L (info.name, List.concat_map (aux labels (not pos)) sub |> remove_redundant)]
    else List.concat_map (aux labels pos) sub
  in
  let lines labels =
    !top_classes |> LabelSet.to_list |> List.concat_map (aux labels true) |> remove_redundant
  in
  let pos, neg, unk = lines pos, lines neg, lines unk in
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
  let rec print_class_line prec assoc fmt (L (str, t)) =
    match t with
    | [] -> Format.fprintf fmt "%s" str
    | t ->
      let print_tuple prec assoc fmt t = print_nary_op print_class_line prec assoc Tuple fmt t in
      print_binary_op' (print_atomic_str str) print_tuple prec assoc Diff fmt () t
  in
  let _, neg_prec, _ = unop_info Neg in
  let print_line fmt (kind,t) =
    match kind with
    | `Pos -> print_class_line min_prec NoAssoc fmt t
    | `Neg -> print_neg print_class_line min_prec NoAssoc fmt t
    | `Unk -> print_unary print_class_line min_prec NoAssoc (fs "?", neg_prec, NoAssoc) fmt t
  in
  let print suffix fmt t =
    Format.fprintf fmt "%a%s" print_line t suffix
  in
  let print_rv _prec _assoc fmt rv = RowVar.pp fmt rv in
  let print_tail fmt t =
    match t with
    | NoOther -> ()
    | AllOthers -> Format.fprintf fmt "*"
    | Unknown -> Format.fprintf fmt "..."
    | RowVars dnf ->
      Pp.print_non_empty_dnf' ~cmp:RowVar.compare print_rv Prec.min_prec NoAssoc fmt dnf
  in
  let bindings =
    (pos |> List.map (fun p -> `Pos,p))@
    (neg |> List.map (fun n -> `Neg,n))@
    (unk |> List.map (fun n -> `Unk,n))
  in
  match tail with
  | NoOther ->
    Format.fprintf fmt "<%a>" (print_seq (print "") ", ") bindings
  | AllOthers | Unknown | RowVars _ ->
    Format.fprintf fmt "<%a%a>" (print_seq (print ", ") "") bindings print_tail tail

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun _ _ v -> v) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
