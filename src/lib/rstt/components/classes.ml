open Sstt

type 'r tail =
| NoOther | AllOthers | Unknown
| RowVars of ('r list * 'r list) list
type 'r atom = { pos:attrs ; neg:attrs ; unk:string list ; tail:'r tail }
and attrs = line list
and line = L of string * attrs

module LabelSet = Set.Make(Label)

(* [supers] and [subs] are the direct relations, while [up] (resp. [down]) is the
   set of all the superclasses (resp. subclasses) of a class, itself included.
   The class lattice must be built before building any type that mentions a
   class: after that, relations cannot be added anymore. *)
type label_info = { name:string; supers: LabelSet.t; subs: LabelSet.t;
                    up: LabelSet.t; down: LabelSet.t }
let labels = Hashtbl.create 10
let infos = Hashtbl.create 10
let built = ref false
let tag = Tag.mk "class"

let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

let label_of_class name =
  match Hashtbl.find_opt labels name with
  | Some lbl -> lbl
  | None ->
    let lbl = Label.mk (Utils.slugify name) in
    let self = LabelSet.singleton lbl in
    Hashtbl.add infos lbl { name ; supers=LabelSet.empty ; subs=LabelSet.empty ; up=self ; down=self } ;
    Hashtbl.add labels name lbl ;
    lbl

let add_relation name ~supers =
  if !built then invalid_arg "Class lattice already built" ;
  let lbl = label_of_class name in
  let supers = supers |> List.map label_of_class |> LabelSet.of_list in
  let info = Hashtbl.find infos lbl in
  Hashtbl.replace infos lbl { info with supers = LabelSet.union info.supers supers }

let build_lattice () =
  if not !built then begin
    let info lbl = Hashtbl.find infos lbl in
    let lbls = Hashtbl.to_seq_keys infos |> List.of_seq in
    let ups = Hashtbl.create 10 in
    let rec up visiting lbl =
      match Hashtbl.find_opt ups lbl with
      | Some up -> up
      | None ->
        if LabelSet.mem lbl visiting then invalid_arg "Cyclic class hierarchy" ;
        let visiting = LabelSet.add lbl visiting in
        let up = (info lbl).supers |> LabelSet.to_list |> List.map (up visiting)
                 |> List.fold_left LabelSet.union (LabelSet.singleton lbl) in
        Hashtbl.add ups lbl up ; up
    in
    let up = up LabelSet.empty in
    (* Only keep the direct superclasses *)
    let direct lbl =
      let supers = (info lbl).supers in
      supers |> LabelSet.filter (fun s ->
          supers |> LabelSet.exists (fun s' ->
              Label.equal s s' |> not && LabelSet.mem s (up s')) |> not)
    in
    let supers = lbls |> List.map (fun lbl -> lbl, direct lbl) in
    let subs lbl = supers |> List.filter_map (fun (lbl', supers) ->
        if LabelSet.mem lbl supers then Some lbl' else None) |> LabelSet.of_list in
    let down lbl = lbls |> List.filter (fun lbl' -> LabelSet.mem lbl (up lbl')) |> LabelSet.of_list in
    supers |> List.iter (fun (lbl, supers) ->
        let info = { (info lbl) with supers ; subs = subs lbl ; up = up lbl ; down = down lbl } in
        Hashtbl.replace infos lbl info) ;
    built := true
  end

let class_label name =
  if not !built then invalid_arg "Class lattice not built" ;
  label_of_class name

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

let up info = info.up
let down info = info.down
let rec labels_of_attrs closure lst =
  lst |> List.map (fun (L (str,l)) ->
      let all = closure (Hashtbl.find infos (class_label str)) in
      LabelSet.diff all (labels_of_attrs closure l)
    ) |> List.fold_left LabelSet.union LabelSet.empty

let tt, ff = Enum.mk "tt", Enum.mk "ff"
let tt, ff = Descr.mk_enum tt |> Ty.mk_descr, Descr.mk_enum ff |> Ty.mk_descr
let bb = Ty.cup tt ff
let mk {pos;neg;unk;tail} =
  (* Positive classes also constrain their superclasses, negated classes their
     subclasses, and unknown classes only themselves. Constraints on a same
     label are intersected. *)
  let pos, neg = labels_of_attrs up pos, labels_of_attrs down neg in
  let unk = unk |> List.map class_label |> LabelSet.of_list in
  let constrs = [ pos, tt ; neg, ff ; unk, bb ] in
  let field lbl =
    constrs |> List.filter_map (fun (lbls, ty) ->
        if LabelSet.mem lbl lbls then Some ty else None)
    |> Ty.conj |> Ty.O.required |> Ty.F.mk_descr
  in
  let bindings = LabelSet.union pos (LabelSet.union neg unk) |> LabelSet.to_list
                 |> List.map (fun lbl -> lbl, field lbl) |> LabelMap.of_list in
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
let remove_redundant closure lines =
  let union ds = List.fold_left LabelSet.union LabelSet.empty ds in
  let rec aux kept lines =
    match lines with
    | [] -> List.rev kept
    | (l,d)::lines ->
      let others = union (List.map snd kept @ List.map snd lines) in
      if LabelSet.subset d others then aux kept lines else aux ((l,d)::kept) lines
  in
  lines |> List.map (fun l -> l, labels_of_attrs closure [l]) |> aux [] |> List.map fst

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
  (* Positive lines are built from the most specific classes towards their
     superclasses, and negative lines the other way around *)
  let lines closure prev next labels =
    let rec aux pos lbl =
      let info = Hashtbl.find infos lbl in
      let next = next info |> LabelSet.to_list in
      if (LabelSet.mem lbl labels) = pos
      then [L (info.name, List.concat_map (aux (not pos)) next |> remove_redundant closure)]
      else List.concat_map (aux pos) next
    in
    let first = Hashtbl.to_seq infos |> Seq.filter_map (fun (lbl, info) ->
        if LabelSet.is_empty (prev info) then Some lbl else None)
      |> List.of_seq |> List.sort Label.compare in
    first |> List.concat_map (aux true) |> remove_redundant closure
  in
  let pos = lines up (fun info -> info.subs) (fun info -> info.supers) pos in
  let neg = lines down (fun info -> info.supers) (fun info -> info.subs) neg in
  let unk = unk |> LabelSet.to_list |> List.map (fun lbl -> (Hashtbl.find infos lbl).name) in
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
  let print_line fmt t =
    match t with
    | `Pos t -> print_class_line min_prec NoAssoc fmt t
    | `Neg t -> print_neg print_class_line min_prec NoAssoc fmt t
    | `Unk str -> Format.fprintf fmt "?%s" str
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
    (pos |> List.map (fun p -> `Pos p))@
    (neg |> List.map (fun n -> `Neg n))@
    (unk |> List.map (fun u -> `Unk u))
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
