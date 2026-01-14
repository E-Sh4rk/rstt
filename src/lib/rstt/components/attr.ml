open Sstt

let tag = Tag.mk "attr"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type ('a, 'c) atom = { content:'a ; classes:'c }
type ('a, 'c) line = ('a, 'c) atom list * ('a, 'c) atom list
type ('a, 'c) t = ('a, 'c) line list

let content_label = Label.mk "_content"
let class_label = Label.mk "_class"

let mk { content ; classes } =
  let classes = Ty.cap classes Classes.any in
  let bindings = LabelMap.of_list [
    content_label, Ty.F.mk_descr (Ty.O.required content) ;
    class_label, Ty.F.mk_descr (Ty.O.required classes) ] in
  { Records.Atom.bindings ; tail=Ty.F.mk_descr Ty.O.absent } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk_anyclass content =
  mk { content ; classes=Classes.any }
let mk_noclass content =
  mk { content ; classes=Classes.noclass }
let any_d =
  let bindings = LabelMap.of_list [
    content_label, Ty.F.mk_descr (Ty.O.required Ty.any) ;
    class_label, Ty.F.mk_descr (Ty.O.required Classes.any) ] in
  { Records.Atom.bindings ; tail=Ty.F.mk_descr Ty.O.absent } |> Descr.mk_record |> Ty.mk_descr
let any = add_tag any_d

let map_atom f fc { content ; classes } = { content=f content ; classes=fc classes }
let map_line f fc (ps,ns) = (List.map (map_atom f fc) ps, List.map (map_atom f fc) ns)
let map f fc (l : ('a,'b) t) = l |> List.map (map_line f fc)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid attr encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom r =
  let content = Records.Atom.find content_label r |> Ty.F.get_descr |> Ty.O.get in
  let classes = Records.Atom.find class_label r  |> Ty.F.get_descr |> Ty.O.get in
  { content ; classes }
let extract t : (Ty.t, Ty.t) t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build ctx.Printer.build)
  else None

let destruct ty = proj_tag ty |> extract

let proj_content ty =
  proj_tag ty |> Ty.get_descr |> Descr.get_records |> Op.Records.proj content_label |> Ty.O.get
let proj_classes ty =
  proj_tag ty |> Ty.get_descr |> Descr.get_records |> Op.Records.proj class_label |> Ty.O.get

let print prec assoc fmt t =
  let print_atom prec assoc fmt { content ; classes } =
    let print_opt_content fmt t =
      if Ty.is_any t.Printer.ty |> not then Printer.print_descr_atomic fmt t
    in
    if Ty.leq Classes.any classes.Printer.ty then
      Format.fprintf fmt "%a" (Printer.print_descr_ctx prec assoc) content
    else
      Format.fprintf fmt "%a%a" print_opt_content content Printer.print_descr classes
  in
  Prec.print_non_empty_dnf ~any:"any" print_atom prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map f f) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
