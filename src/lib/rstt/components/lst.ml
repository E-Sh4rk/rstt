open Sstt
open Rstt_utils

let tag = Tag.mk "lst"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type 'a atom = 'a list * (string * 'a) list * 'a
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

let mk (pos, named, tail) =
  let pos = List.mapi (fun i ty -> Labels.pos i, ty) pos in
  let named = List.map (fun (str,ty) -> Labels.named str, ty) named in
  let bindings = LabelMap.of_list (pos@named) in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let any_d = Records.any |> Descr.mk_records |> Ty.mk_descr
let any = add_tag any_d

let map_atom f (pos,named,tl) = List.map f pos, List.map (fun (str,t) -> str, f t) named, f tl
let map_line f (ps,ns) = (List.map (map_atom f) ps, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid list encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom { Records.Atom.bindings ; tail } =
  let pos, named = bindings |> LabelMap.bindings |> List.partition_map (fun (lbl,ty) ->
    match Labels.info lbl with
    | Pos i -> Either.left (i,ty)
    | Named str -> Either.right (str,ty)
    | PosNamed _ -> invalid_arg "Invalid list encoding."
    ) in
  let pos = List.sort (fun t1 t2 -> Stdlib.compare (fst t1) (fst t2)) pos in
  let pos = List.map snd pos in
  pos, named, tail
let extract t : Ty.F.t t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_fop)
  else None

let destruct ty = proj_tag ty |> extract

let print prec assoc fmt t =
  let print_pos_atom fmt (pos,named,tail) =
    let print_field_ty = Printer.print_field_ctx Prec.min_prec Prec.NoAssoc in
    let print_field fmt (name,ty) =
      match name with
      | None -> Format.fprintf fmt "%a" print_field_ty ty
      | Some str -> Format.fprintf fmt "%s: %a" str print_field_ty ty
    in
    let pos, named = List.map (fun t -> None, t) pos, List.map (fun (str,t) -> Some str, t) named in
    Format.fprintf fmt "{ %a ; %a }" (print_seq print_field ", ") (pos@named) print_field_ty tail
  in
  let print_neg_atom prec assoc fmt a =
    let sym,_,_ as opinfo = Prec.unop_info Neg in
    Prec.fprintf prec assoc opinfo fmt "%s%a" sym print_pos_atom a
  in
  let print_atom prec assoc fmt (pos,a) =
    if pos then print_pos_atom fmt a
    else print_neg_atom prec assoc fmt a
  in
  let print_line prec assoc fmt (ps, ns) =
    let ps, ns = List.map (fun p -> true,p) ps, List.map (fun n -> false,n) ns in
    let sym,prec',_ as opinfo = Prec.varop_info Cap in
    (* TODO: no paren if only one *)
    Prec.fprintf prec assoc opinfo fmt "%a" (print_seq (print_atom prec' NoAssoc) sym) (ps@ns)
  in
  (* TODO: no paren if only one *)
  let sym,prec',_ as opinfo = Prec.varop_info Cup in
  Prec.fprintf prec assoc opinfo fmt "%a" (print_seq (print_line prec' NoAssoc) sym) t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map (Printer.map_fop f)) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
