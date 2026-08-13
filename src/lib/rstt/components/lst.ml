open Sstt
open Rstt_utils

let tag = Tag.mk "lst"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type ('l,'a) atom = { bindings: ('l * 'a) list ; tl:'a }
type ('l,'a) line = ('l,'a) atom list * ('l,'a) atom list
type ('l,'a) t = ('l,'a) line list

let mk { bindings ; tl } =
  let bindings = List.map (fun (str,ty) -> Labels.named str, ty) bindings in
  let bindings = LabelMap.of_list bindings in
  let tail = Utils.add_option tl in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let any = mk {bindings=[]; tl=Ty.F.any}
let any_d = proj_tag any
let empty = mk {bindings=[]; tl=Ty.F.mk_descr Ty.O.absent}

let map_atom fl f {bindings;tl} =
  { bindings=List.map (fun (lbl,t) -> fl lbl, f t) bindings ; tl=f tl }
let map_line f (ps,ns) = (List.map (map_atom Fun.id f) ps, List.map (map_atom Fun.id f) ns)
let map f (l : ('l,'a) t) = l |> List.map (map_line f)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid list encoding." ;
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom { Records.Atom.bindings ; tail } =
  let bindings = bindings |> LabelMap.bindings |> List.map (fun (lbl,ty) ->
    match Labels.info lbl with
    | Named str -> (str,ty)
    | Pos _ -> assert false)
  in
  { bindings ; tl=tail }
let extract t : (string, Ty.F.t) t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_field)
  else None

let destruct ty = proj_tag ty |> Ty.cap any_d |> extract
let proj lbl ty =
  proj_tag ty |> Ty.cap any_d |> Ty.get_descr |> Descr.get_records
  |> Op.Records'.proj (Labels.named lbl)

let print prec assoc fmt t =
  let cmp {bindings=b1;tl=t1} {bindings=b2;tl=t2} =
    let open Rstt_utils in
    let cmp_field (str1,f1) (str2,f2) =
      String.compare str1 str2 |> ccmp Pp.Compare.fdescr f1 f2
    in
    let cmp_bindings b1 b2 = List.compare cmp_field b1 b2 in
    Pp.Compare.fdescr t1 t2 |> ccmp cmp_bindings b1 b2
  in
  let is_absent fd = Ty.F.equiv fd.Printer.fty (Ty.F.mk_descr Ty.O.absent) in
  let print_field_ty fmt f =
    match f with
    | f when is_absent f -> Format.fprintf fmt "absent"
    | f -> Printer.print_field_ctx Prec.min_prec Prec.NoAssoc fmt f
  in
  let print_field suffix fmt (str,ty) =
    Format.fprintf fmt "%s: %a%s" str print_field_ty ty suffix
  in
  let print_atom _prec _assoc fmt {bindings;tl} =
    match tl with
    | tl when is_absent tl ->
      Format.fprintf fmt "{ %a }" (print_seq (print_field "") ", ") bindings
    | _ ->
      Format.fprintf fmt "{ %a%a }" (print_seq (print_field ", ") "") bindings
        print_field_ty (Utils.prune_option_fdescr tl)
  in
  Pp.print_non_empty_dnf ~cmp ~any:"list" print_atom prec assoc fmt t
let print = Utils.struct_print print

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun _ ff -> map ff) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
