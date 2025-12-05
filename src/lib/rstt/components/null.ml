open Sstt

let tag = Tag.mk "null"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let any_p = Ty.any
let any = add_tag any_p

let to_t _ _ comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
  then Some ()
  else None

let map _f v = v
let print _ _ fmt () = Format.fprintf fmt "null"

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
