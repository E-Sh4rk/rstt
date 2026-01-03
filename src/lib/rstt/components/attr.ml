open Sstt

let tag = Tag.mk "attr"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type 'a atom = { content:'a ; classes:'a }
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

let content_label = Label.mk "_content"
let pclass_label = Label.mk "_pclass"
let nclass_label = Label.mk "_nclass"

let mk { content ; classes } =
  let bindings = LabelMap.of_list [
    content_label, Ty.F.mk_descr (Ty.O.required content) ;
    pclass_label, Ty.F.mk_descr (Ty.O.optional classes) ;
    nclass_label, Ty.F.mk_descr (Ty.neg classes |> Ty.O.optional) ] in
  { Records.Atom.bindings ; tail=Ty.F.mk_descr Ty.O.absent } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk_noclass content = mk { content ; classes=Ty.empty }
let any_d =
  let bindings = LabelMap.of_list [
    content_label, Ty.F.mk_descr (Ty.O.required Ty.any) ;
    pclass_label, Ty.F.mk_descr (Ty.O.optional Ty.any) ;
    nclass_label, Ty.F.mk_descr (Ty.O.optional Ty.any) ] in
  { Records.Atom.bindings ; tail=Ty.F.mk_descr Ty.O.absent } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let any = add_tag any_d

let map_atom f { content ; classes } = { content=f content ; classes=f classes }
let map_line f (ps,ns) = (List.map (map_atom f) ps, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid attr encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom r =
  let content = Records.Atom.find content_label r |> Ty.F.get_descr |> Ty.O.get in
  let classes = Records.Atom.find pclass_label r  |> Ty.F.get_descr |> Ty.O.get in
  { content ; classes }
let extract t : Ty.t t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build)
  else None

let destruct ty = proj_tag ty |> extract

let print prec assoc fmt t =
  let print_atom prec assoc fmt { content ; classes=_ } =
    (* TODO *)
    Format.fprintf fmt "%a" (Printer.print_descr_ctx prec assoc) content
  in
  Prec.print_non_empty_dnf ~any:"list" print_atom prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
