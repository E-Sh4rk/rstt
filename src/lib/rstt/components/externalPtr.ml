open Sstt
module Reserved = Labels.Reserved

let tag = Tag.mk "eptr"
let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                |> Op.TagComp.as_atom |> snd

let mk' ty =
  let open Records.Atom in
  let bindings = [Reserved.target, ty |> Ty.O.Atom.optional |> Ty.O.mk |> Ty.F.mk_descr] |> LabelMap.of_list in
  let ty = Descr.mk_record { bindings ; tail=Ty.F.any } |> Ty.mk_descr in
  add_tag ty
let any = mk' Ty.any
let null = mk' Ty.empty
let mk ty = mk' ty

type 'a t = { nullable:bool ; target:'a }
let map f { nullable ; target } = { nullable ; target=f target }
let extract ty =
  let oty = Ty.get_descr ty |> Descr.get_records |> Op.Records.approx
  |> Op.Records.Atom.find Reserved.target |> Ty.O.get in
  let nullable, target = Ty.O.Atom.is_optional oty, Ty.O.Atom.get oty in
  { nullable ; target }

let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  Some (extract pty |> map ctx.Printer.build)
let destruct ty = ty |> proj_tag |> extract
let print prec assoc fmt { nullable ; target } =
  let pp_target _ _ fmt target =
    if Ty.is_any target.Printer.ty
    then Format.fprintf fmt "externalptr"
    else Format.fprintf fmt "externalptr(%a)" Pp.print_descr target
  in
  if nullable then
    pp_target prec assoc fmt target
  else
    Prec.print_binary_op' pp_target (Prec.print_atomic_str "externalptr(empty)")
      prec assoc Diff fmt target ()


let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
