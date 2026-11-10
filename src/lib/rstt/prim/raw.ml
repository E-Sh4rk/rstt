open Sstt

(* Raw vectors cannot contain NA: there is no NA component, and any = any' *)
let tag = Tag.mk "raw"
let any_p = Descr.mk_enum (Enum.mk "_") |> Ty.mk_descr
let any = (tag, any_p) |> Descr.mk_tag |> Ty.mk_descr
let any' = any
let any_sub, any_sub' = any, any'

let is_singleton _ = false

let to_t _ comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
  then Some () else None
let print _ _ fmt () =
  let suffix = match Pp.current_pos () with Pp.Prim s -> s | _ -> "" in
  Format.fprintf fmt "RAW%s" suffix

let printer_builder = Printer.builder ~to_t ~map:(fun _ _ v -> v) ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
