open Sstt
module Reserved = Labels.Reserved

module PtrStar = struct
  let sym () = format_of_string "*"
  let prec = 5
  let assoc = Prec.NoAssoc
  let opinfo () = (sym (), prec, assoc)
end

let tag = Tag.mk "ptr"
let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                |> Op.TagComp.as_atom |> snd
let mk ty =
  let open Records.Atom in
  let bindings =  [Reserved.target, ty |> Ty.O.optional |> Ty.F.mk_descr] |> LabelMap.of_list in
  let ty = Descr.mk_record { bindings ; tail=Ty.F.any } |> Ty.mk_descr in
  add_tag ty
let any = mk Ty.any
let null = mk Ty.empty

type 'a t = { nullable:bool ; target:'a }
let map f { nullable ; target } = { nullable ; target=f target }
let extract ty =
  let oty = Ty.get_descr ty |> Descr.get_records |> Op.Records.approx
  |> Op.Records.Atom.find Reserved.target in
  { nullable=Ty.O.is_optional oty ; target=Ty.O.get oty }

let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  Some (extract pty |> map ctx.Printer.build)
let destruct ty = ty |> proj_tag |> extract
let print prec assoc fmt { nullable ; target } =
  let ((sym, prec', _) as opinfo) = PtrStar.opinfo () in
  let pp_target prec assoc fmt target =
    Prec.fprintf prec assoc opinfo fmt "%(%)%a" sym
      (Pp.print_descr_ctx prec' NoAssoc) target
  in
  if nullable then
    if Ty.is_empty target.Printer.ty then
      Format.fprintf fmt "c_null"
    else
      pp_target prec assoc fmt target
  else
    let sym,prec',_ as opinfo = Prec.binop_info Diff in
    Prec.fprintf prec assoc opinfo fmt "%a%(%)%s" (pp_target prec' Right) target sym "c_null"


let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
