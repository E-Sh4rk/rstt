open Sstt

module P = struct
  let tag_name = "raw"
  let any = Descr.mk_enum (Enum.mk "_") |> Ty.mk_descr

  type t = unit
  let any_t = ()
  let to_t _ ty =
    if Ty.leq ty any then Some () else None
  let is_singleton _ = false
  let may_not_feature_any _ = false

  let print prefix suffix _ _ fmt () = Format.fprintf fmt "%sRAW%s" prefix suffix
end

include Na.MakeCompWithNa(P)
let any_sub, any_sub' = any, any'

let printer_builder = Printer.builder ~to_t ~map:(fun _ _ v -> v) ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params