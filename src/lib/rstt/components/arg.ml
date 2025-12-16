open Sstt
open Rstt_utils

let tag = Tag.mk "arg"
let npos_label = Label.mk "_npos"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd
let npos_field n = npos_label, Intervals.Atom.mk_singl n |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr
let npos_field' n = npos_label, Intervals.Atom.mk (Some n) None |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr

type 'a atom = { pos : 'a list ; pos_named : (string * 'a) list ; tl : 'a ; named : (string * 'a) list }
type 'a atom' = { pos' : 'a list ; tl' : 'a ; named' : (string * 'a) list }
type 'a t =
| DefSite of 'a atom list
| CallSite of 'a atom' list

let split_at_index lst n =
  let rec aux acc next n =
    if n = 0 then List.rev acc, next
    else match next with
    | [] -> assert false
    | p::defs -> aux (p::acc) defs (n-1)
  in
  aux [] lst n

let mk' ~allow_more_pos { pos' ; tl' ; named' } =
  let more_pos = allow_more_pos && (Ty.F.get_descr tl' |> Ty.O.get |> Ty.is_empty |> not) in
  let n = List.length pos' |> Z.of_int in
  let pos = pos' |> List.mapi (fun i fty -> Labels.pos i, fty) in
  let named = named' |> List.map (fun (str, fty) -> Labels.named str, fty) in
  let npos = if more_pos then npos_field' n else npos_field n in
  let bindings = npos::pos@named |> LabelMap.of_list in
  { Records.Atom.bindings ; tail=tl' } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk { pos ; pos_named ; tl ; named } =
  let n = List.length pos_named in
  let atoms' = List.init (n + 1) (fun i ->
    let pos', named' = split_at_index pos_named i in
    let pos' = pos@(pos' |> List.map (fun (_,fty) -> fty)) in
    let named' = named@named' in
    mk' ~allow_more_pos:(i=n) { pos' ; named' ; tl'=tl }
  ) in
  let model_npos = npos_field (Z.minus_one) in
  let model_pos = pos |> List.mapi (fun i fty -> Labels.pos i, fty) in
  let model_pos_named = pos_named |> List.mapi (fun i (str,fty) -> Labels.pos_named (i,str), fty) in
  let model_named = named |> List.map (fun (str,fty) -> Labels.named str, fty) in
  let model_bindings = model_npos::(List.concat [model_pos ; model_pos_named ; model_named]) |> LabelMap.of_list in
  let model = { Records.Atom.bindings=model_bindings ; tail=tl } |> Descr.mk_record |> Ty.mk_descr |> add_tag in
  model::atoms' |> Ty.disj
let mk' = mk' ~allow_more_pos:true
let any_d =
  { Records.Atom.bindings=[npos_field' (Z.minus_one)] |> LabelMap.of_list ; tail=Ty.F.any }
  |> Descr.mk_record |> Ty.mk_descr
let any = add_tag any_d

let map_atom f { pos ; pos_named ; tl ; named } =
  let pos = List.map f pos in
  let pos_named = List.map (fun (str,t) -> str, f t) pos_named in
  let named = List.map (fun (str,t) -> str, f t) named in
  let tl = f tl in
  { pos ; pos_named ; named ; tl }
let map_atom' f { pos' ; tl' ; named' } =
  let pos' = List.map f pos' in
  let named' = List.map (fun (str,t) -> str, f t) named' in
  let tl' = f tl' in
  { pos' ; named' ; tl' }
let map f t =
  match t with
  | DefSite lst -> DefSite (List.map (map_atom f) lst)
  | CallSite lst -> CallSite (List.map (map_atom' f) lst)

let extract ty : Ty.F.t t =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid arg encoding." ;
  let extract_defsite a =
    let bindings = a.Records.Atom'.bindings |> LabelMap.bindings |>
      List.filter_map (fun (lbl,ty) -> try Some (Labels.info lbl, ty) with Invalid_argument _ -> None) in
    let pos = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.Pos i -> Some (i,ty) | _ -> None) in
    let named = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.Named str -> Some (str,ty) | _ -> None) in
    let pos_named = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.PosNamed (i,str) -> Some ((i,str),ty) | _ -> None) in
    let pos = List.sort (fun (i1,_) (i2,_) -> Stdlib.Int.compare i1 i2) pos |> List.map snd in
    let pos_named = List.sort (fun ((i1,_),_) ((i2,_),_) -> Stdlib.Int.compare i1 i2) pos_named |> List.map (fun ((_,str),ty) -> str, ty) in
    { pos ; pos_named ; tl=a.Records.Atom'.tail ; named }
  in
  let extract_callsite a =
    let bindings = a.Records.Atom'.bindings |> LabelMap.bindings |>
      List.filter_map (fun (lbl,ty) -> try Some (Labels.info lbl, ty) with Invalid_argument _ -> None) in
    let pos' = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.Pos i -> Some (i,ty) | _ -> None) in
    let named' = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.Named str -> Some (str,ty) | _ -> None) in
    let pos' = List.sort (fun (i1,_) (i2,_) -> Stdlib.Int.compare i1 i2) pos' |> List.map snd in
    { pos' ; tl'=a.Records.Atom'.tail ; named' }
  in
  let lines = Ty.get_descr ty |> Descr.get_records |> Records.dnf' in
  let models = lines |> List.filter (fun a -> Ty.leq
    (Intervals.Atom.mk_singl Z.minus_one |> Descr.mk_interval |> Ty.mk_descr)
    (Records.Atom'.find npos_label a |> Ty.F.get_descr |> Ty.O.get)) in
  if models <> []
  then DefSite (List.map extract_defsite models)
  else CallSite (List.map extract_callsite lines)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_fop)
  else None

let destruct ty =
  proj_tag ty |> extract

let print prec assoc fmt t =
  let print_field_ty = Printer.print_field_ctx Prec.min_prec Prec.NoAssoc in
  let print_field fmt (name,ty) =
      match name with
      | None -> Format.fprintf fmt "%a" print_field_ty ty
      | Some str -> Format.fprintf fmt "%s: %a" str print_field_ty ty
  in
  let print_atom fmt a =
    let pos, named, pos_named =
      List.map (fun t -> None, t) a.pos,
      List.map (fun (str,t) -> Some str, t) a.named,
      List.map (fun (str,t) -> Some str, t) a.pos_named in
    Format.fprintf fmt "( %a ; %a ; %a )" (print_seq print_field ", ")
      (pos@pos_named) print_field_ty a.tl (print_seq print_field ", ") named
  in
  let print_atom' fmt a =
    let pos, named =
      List.map (fun t -> None, t) a.pos',
      List.map (fun (str,t) -> Some str, t) a.named' in
    Format.fprintf fmt "@( %a ; %a ; %a )" (print_seq print_field ", ")
      pos print_field_ty a.tl' (print_seq print_field ", ") named
  in
  let sym,_,_ as opinfo = Prec.varop_info Cup in
  match t with
  | DefSite t -> Prec.fprintf prec assoc opinfo fmt "%a" (print_seq print_atom sym) t
  | CallSite t -> Prec.fprintf prec assoc opinfo fmt "%a" (print_seq print_atom' sym) t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map (Printer.map_fop f)) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
