open Sstt

(* A tail [Tail (kind, dnf)] denotes the union of the row variable formula [dnf]
   (a DNF of positive and negative row variables) with: all the other classes
   for [PresentOr], none of them for [AbsentOr], and nothing for [Exactly].
   [Unknown] leaves the other classes unspecified. *)
type tail_kind = PresentOr | AbsentOr | Exactly
type 'r tail =
| Unknown
| Tail of tail_kind * ('r list * 'r list) list
(* Method classes are written [@m], and classes [c]. Names that are not
   identifiers are quoted: [@"m"] and ["c"]. *)
type cls = Class of string | Method of string
type 'r atom = { pos:attrs ; neg:cls list ; unk:attrs ; tail:'r tail }
and attrs = line list
and line = L of cls * attrs

let is_ident str =
  let is_start c = match c with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false in
  let is_char c = is_start c || match c with '0'..'9' | '\'' | '.' -> true | _ -> false in
  let n = String.length str in
  let from i = i < n && is_start str.[i] && String.for_all is_char (String.sub str i (n-i)) in
  from 0 || (n > 0 && str.[0] = '.' && from 1)
let quote str = if is_ident str then str else Printf.sprintf "%S" str
let unquote str =
  if str <> "" && str.[0] = '"' then
    try Scanf.sscanf str "%S%!" Fun.id
    with Scanf.Scan_failure _ | Failure _ | End_of_file -> invalid_arg "Invalid class name"
  else str
let string_of_cls cls =
  match cls with
  | Class c -> quote c
  | Method m -> "@"^(quote m)
let cls_of_string str =
  if str <> "" && str.[0] = '@'
  then Method (String.sub str 1 (String.length str - 1) |> unquote)
  else Class (unquote str)
let pp_cls fmt cls = Format.fprintf fmt "%s" (string_of_cls cls)

module LabelSet = Set.Make(Label)

(* Each class (resp. method class) has a label. A class implies the method
   classes of the methods it implements: they must be given when declaring it,
   before it is used. Method classes and undeclared classes are created on the
   fly. *)
type label_info = { cls:cls ; methods:LabelSet.t }
let labels = Hashtbl.create 10
let infos = Hashtbl.create 10
let tag = Tag.mk "class"

let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

let new_label cls methods =
  let lbl = Label.mk (Utils.slugify (string_of_cls cls)) in
  Hashtbl.add infos lbl { cls ; methods } ; Hashtbl.add labels cls lbl ;
  lbl

let label_of_cls cls =
  match Hashtbl.find_opt labels cls with
  | Some lbl -> lbl
  | None -> new_label cls LabelSet.empty

let declare_class name ~methods =
  let cls = Class name in
  if Hashtbl.mem labels cls then invalid_arg "Class already declared or used" ;
  let methods = methods |> List.map (fun m -> label_of_cls (Method m)) |> LabelSet.of_list in
  new_label cls methods |> ignore

let union = List.fold_left LabelSet.union LabelSet.empty
let up lbl = LabelSet.add lbl (Hashtbl.find infos lbl).methods

let map_tail f t =
  match t with
  | Unknown -> Unknown
  | Tail (k, lst) -> Tail (k, List.map (fun (ps,ns) -> List.map f ps, List.map f ns) lst)
let map_atom f ({pos;neg;unk;tail}:'a atom) : 'b atom = {pos;neg;unk;tail=map_tail f tail}

let rec labels_of_attrs lst =
  lst |> List.map (fun (L (cls,l)) ->
      LabelSet.diff (label_of_cls cls |> up) (labels_of_attrs l)
    ) |> union

let tt, ff = Enum.mk "tt", Enum.mk "ff"
let tt, ff = Descr.mk_enum tt |> Ty.mk_descr, Descr.mk_enum ff |> Ty.mk_descr
let bb = Ty.cup tt ff
let mk {pos;neg;unk;tail} =
  (* Positive and unknown classes also constrain their methods, while negated
     classes only constrain themselves. Constraints on a same label are
     intersected. *)
  let pos, unk = labels_of_attrs pos, labels_of_attrs unk in
  let neg = neg |> List.map label_of_cls |> LabelSet.of_list in
  let constrs = [ pos, tt ; neg, ff ; unk, bb ] in
  let field lbl =
    constrs |> List.filter_map (fun (lbls, ty) ->
        if LabelSet.mem lbl lbls then Some ty else None)
    |> Ty.conj |> Ty.O.required |> Ty.F.mk_descr
  in
  let bindings = LabelSet.union pos (LabelSet.union neg unk) |> LabelSet.to_list
                 |> List.map (fun lbl -> lbl, field lbl) |> LabelMap.of_list in
  let tail = match tail with
  | Unknown -> bb |> Ty.O.required |> Ty.F.mk_descr
  | Tail (kind, dnf) ->
    let others = match kind with
      | PresentOr -> [ [], [], Ty.O.required tt ]
      | AbsentOr -> [ [], [], Ty.O.required ff ]
      | Exactly -> []
    in
    others @ (dnf |> List.map (fun (ps,ns) -> (ps,ns,Ty.O.required bb))) |> Ty.F.of_dnf
  in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag

let any = mk {pos=[];neg=[];unk=[];tail=Unknown}
let any_d = proj_tag any
let noclass = mk {pos=[];neg=[];unk=[];tail=Tail (AbsentOr, [])}

let extract_record ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid attr encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Op.Records'.approx
(* The tail field is a union of monomials [ps & ~ns & leaf]. Variable-free
   monomials give the kind of the tail. A monomial with row variables is absorbed
   when its leaf is covered by the kind; otherwise only the part of its leaf that
   is not covered matters, and that part is a single value ([tt] or [ff]): we can
   replace the leaf by [bb] without changing the union. When the kind is [Exactly],
   the leaves of the monomials are approximated by [bb]. *)
let tail_of_field fty =
  let dnf = Ty.F.dnf fty |> List.map (fun (ps,ns,leaf) ->
      ps, ns, Ty.O.get leaf |> Ty.O.Atom.get) in
  let consts, monomials = dnf |> List.partition (fun (ps,ns,_) -> ps = [] && ns = []) in
  let consts = consts |> List.map (fun (_,_,ty) -> ty) |> Ty.disj in
  let covers ty = Ty.leq ty consts in
  let kind, covered =
    match covers tt, covers ff with
    | true, true -> None, bb
    | true, false -> Some PresentOr, tt
    | false, true -> Some AbsentOr, ff
    | false, false -> Some Exactly, Ty.empty
  in
  match kind with
  | None -> Unknown
  | Some kind ->
    let dnf = monomials |> List.filter_map (fun (ps,ns,ty) ->
        if Ty.leq ty covered then None else Some (ps,ns)) in
    Tail (kind, dnf)

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
  (* TODO: class fields that mention row variables are approximated *)
  let values = bindings |> List.map (fun (lbl, fty) ->
      let ty = ty_of_field fty in
      lbl, if Ty.leq ty tt then `T else if Ty.leq ty ff then `F else `B) in
  let tail = tail_of_field r.tail in
  (* Value of the labels that are not mentioned, if it does not depend on row
     variables *)
  let tail_value =
    match tail with
    | Unknown -> Some `B
    | Tail (PresentOr, []) -> Some `T
    | Tail (AbsentOr, []) -> Some `F
    | Tail _ -> None
  in
  let value lbl =
    match List.assoc_opt lbl values with
    | Some v -> Some v
    | None -> tail_value
  in
  let cls lbl = (Hashtbl.find infos lbl).cls in
  let classes v = values |> List.filter_map (fun (lbl, v') ->
      match cls lbl with Class _ when v = v' -> Some lbl | _ -> None) in
  let line excluded lbl =
    let excs = (Hashtbl.find infos lbl).methods |> LabelSet.filter excluded in
    L (cls lbl, excs |> LabelSet.to_list |> List.map (fun m -> L (cls m, []))),
    LabelSet.diff (up lbl) excs
  in
  (* Positive classes, without their methods that are not present. Unknown
     classes, without their methods whose value depends on row variables: the
     value of the other ones can be fixed by combining constraints. *)
  let pos, tts = classes `T |> List.map (line (fun m -> value m <> Some `T)) |> List.split in
  let unk, bbs = classes `B |> List.map (line (fun m -> value m = None)) |> List.split in
  let tts, bbs = union tts, union bbs in
  (* Then, we fix the labels whose value is not the expected one *)
  let current lbl =
    if LabelSet.mem lbl tts then Some `T
    else if LabelSet.mem lbl bbs then Some `B
    else tail_value
  in
  let fixes = values |> List.map fst |> LabelSet.of_list |> LabelSet.union bbs
              |> LabelSet.to_list |> List.filter_map (fun lbl ->
                  if current lbl = value lbl then None
                  else Option.map (fun v -> v, cls lbl) (value lbl)) in
  let fixes v = fixes |> List.filter_map (fun (v',cls) -> if v = v' then Some cls else None) in
  let pos = pos @ (fixes `T |> List.map (fun cls -> L (cls, []))) in
  let unk = unk @ (fixes `B |> List.map (fun cls -> L (cls, []))) in
  {pos ; neg=fixes `F ; unk ; tail}

let extract t : RowVar.t atom =
  extract_record t |> record_to_atom
let to_t _ comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then
    let a = extract ty in
    if !Config.safe_mode && not (Ty.equiv (mk a |> proj_tag) ty) then None
    else Some a
  else None

let destruct ty = proj_tag ty |> extract

let print _prec _assoc fmt {pos;neg;unk;tail} =
  let open Sstt.Prec in
  let rec print_class_line prec assoc fmt (L (cls, t)) =
    let str = string_of_cls cls in
    match t with
    | [] -> Format.fprintf fmt "%s" str
    | t ->
      let print_tuple prec assoc fmt t = print_nary_op print_class_line prec assoc Tuple fmt t in
      print_binary_op' (print_atomic_str str) print_tuple prec assoc Diff fmt () t
  in
  let _, neg_prec, _ = unop_info Neg in
  let print_line fmt t =
    match t with
    | `Pos t -> print_class_line min_prec NoAssoc fmt t
    | `Neg cls -> Format.fprintf fmt "~%a" pp_cls cls
    | `Unk t -> print_unary print_class_line min_prec NoAssoc (fs "?", neg_prec, NoAssoc) fmt t
  in
  let print suffix fmt t =
    Format.fprintf fmt "%a%s" print_line t suffix
  in
  let print_rv _prec _assoc fmt rv = RowVar.pp fmt rv in
  let print_dnf fmt dnf =
    Pp.print_non_empty_dnf' ~cmp:RowVar.compare print_rv Prec.min_prec NoAssoc fmt dnf
  in
  let print_tail fmt t =
    match t with
    | Unknown -> Format.fprintf fmt "..."
    | Tail (AbsentOr, []) -> ()
    | Tail (AbsentOr, dnf) -> Format.fprintf fmt ". | %a" print_dnf dnf
    | Tail (PresentOr, []) -> Format.fprintf fmt "*"
    | Tail (PresentOr, dnf) -> Format.fprintf fmt "* | %a" print_dnf dnf
    | Tail (Exactly, dnf) -> print_dnf fmt dnf
  in
  let bindings =
    (pos |> List.map (fun p -> `Pos p))@
    (neg |> List.map (fun n -> `Neg n))@
    (unk |> List.map (fun u -> `Unk u))
  in
  match tail with
  | Tail (AbsentOr, []) ->
    Format.fprintf fmt "<%a>" (print_seq (print "") ", ") bindings
  | Unknown | Tail _ ->
    Format.fprintf fmt "<%a%a>" (print_seq (print ", ") "") bindings print_tail tail

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun _ _ v -> v) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
