open Sstt

module P = struct
  let tag_name = "lgl"
  let tt = Enum.mk "tt" |> Descr.mk_enum |> Ty.mk_descr
  let ff = Enum.mk "ff" |> Descr.mk_enum |> Ty.mk_descr
  let bool b = if b then tt else ff
  let any = Ty.cup tt ff

  type t = { t : bool ; f : bool }
  let any_t = { t=true; f=true }

  let to_t _ _ ty =
    if Ty.leq ty any then
      let t = Ty.leq tt ty in
      let f = Ty.leq ff ty in
      Some { t; f }
    else None

  let map _f v = v
  let print _ _ fmt {t; f} =
    match t, f with
    | false, false -> assert false
    | true, true -> Format.fprintf fmt "lgl"
    | true, false -> Format.fprintf fmt "tt"
    | false, true -> Format.fprintf fmt "ff"
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params

let tt, ff = mk P.tt, mk P.ff
let tt', ff' = mk' P.tt, mk' P.ff
let bool b = mk (P.bool b)
let bool' b = mk' (P.bool b)
