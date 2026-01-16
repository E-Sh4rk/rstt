open Sstt

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
let mk = add_tag
let any = mk Ty.any
let destruct = proj_tag

let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  Some (ctx.Printer.build pty)
let map f t = f t
let print prec assoc fmt t =
  let ((sym, prec', _) as opinfo) = PtrStar.opinfo () in
  Prec.fprintf prec assoc opinfo fmt "%(%)%a" sym
    (Printer.print_descr_ctx prec' NoAssoc) t

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
