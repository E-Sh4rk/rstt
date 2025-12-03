open Sstt

let tag = Tag.mk "lgl"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let tt_p = Enum.mk "tt" |> Descr.mk_enum |> Ty.mk_descr
let ff_p = Enum.mk "ff" |> Descr.mk_enum |> Ty.mk_descr

let tt = add_tag tt_p
let ff = add_tag ff_p
let bool b = if b then tt else ff

let any_p = Ty.cup tt_p ff_p
let any = add_tag any_p

type t = { t : bool ; f : bool }

let to_t _ _ comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
  then
    let t = Ty.leq tt_p pty in
    let f = Ty.leq ff_p pty in
    Some { t; f }
  else None

let map _f v = v
let print _ _ fmt {t; f} =
  match t, f with
  | false, false -> assert false
  | true, true -> Format.fprintf fmt "lgl"
  | true, false -> Format.fprintf fmt "tt"
  | false, true -> Format.fprintf fmt "ff"

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
