open Sstt

module P = struct
  let tag_name = "dbl"
  let any = Descr.mk_enum (Enum.mk "_") |> Ty.mk_descr

  type t = unit
  let any_t = ()
  let to_t _ ty =
    if Ty.leq ty any then Some () else None

  let map _f v = v
  let print _ _ fmt () = Format.fprintf fmt "dbl"
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params