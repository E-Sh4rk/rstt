open Sstt
module Reserved = Labels.Reserved

let tag = Tag.mk "eptr"
let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                |> Op.TagComp.as_atom |> snd

let mk' ty tag =
  let open Records.Atom in
  let bindings = [Reserved.target, ty |> Ty.O.optional |> Ty.F.mk_descr ;
                  Reserved.tag, tag |> Ty.O.required |> Ty.F.mk_descr] |> LabelMap.of_list in
  let ty = Descr.mk_record { bindings ; tail=Ty.F.any } |> Ty.mk_descr in
  add_tag ty
let any = mk' Ty.any Ty.any
let null = mk' Ty.empty Ty.any
let mk ?(tag=Ty.any) ty = mk' ty tag

type 'a t = { nullable:bool ; target:'a ; tag:'a }
let map f { nullable ; target ; tag } = { nullable ; target=f target ; tag=f tag }
let extract ty =
  let atom = Ty.get_descr ty |> Descr.get_records |> Op.Records.approx in
  let oty = atom |> Op.Records.Atom.find Reserved.target |> Ty.O.get in
  let nullable, target = Ty.O.Atom.is_optional oty, Ty.O.Atom.get oty in
  let tag = atom |> Op.Records.Atom.find Reserved.tag |> Ty.O.get |> Ty.O.Atom.get in
  { nullable ; target ; tag }

let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  Some (extract pty |> map ctx.Printer.build)
let destruct ty = ty |> proj_tag |> extract
let print prec assoc fmt { nullable ; target ; tag } =
  let pp_target _ _ fmt target =
    match Ty.is_any target.Printer.ty, Ty.is_any tag.Printer.ty with
    | true, true -> Format.fprintf fmt "externalptr"
    | false, true -> Format.fprintf fmt "externalptr(%a)" Pp.print_descr target
    | _, false ->
        Format.fprintf fmt "externalptr(%a, %a)" Pp.print_descr target Pp.print_descr tag
  in
  if nullable then
    pp_target prec assoc fmt target
  else
    Prec.print_binary_op' pp_target (Prec.print_atomic_str "externalptr(empty)")
      prec assoc Diff fmt target ()
let print = Utils.struct_print print

let printer_builder = Printer.builder ~to_t ~map:(fun f _ v -> map f v) ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
