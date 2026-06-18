open Sstt

module P = struct
  include Integer
  let tag_name = "int"

  let to_t _ = to_t ?pos:None
  let destruct = destruct ?pos:None

  let print prefix suffix prec assoc fmt lines =
    let any = prefix^"INT"^suffix in
    let var = any in
    let pp_int fmt i = Format.fprintf fmt "%iL" i in
    print ~any ~var ~pp_int prec assoc fmt lines
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params

let int i = mk (P.int i)
let int' i = mk' (P.int i)
let var v = mk (P.var v)
let var' v = mk' (P.var v)
let interval i = mk (P.interval i)
let interval' i = mk' (P.interval i)
let bounded i = mk (P.bounded i)
let bounded' i = mk' (P.bounded i)
let destruct ty =
  match destruct ty with
  | Na -> true, []
  | WithNa ty -> true, P.destruct ty
  | WithoutNa ty -> false, P.destruct ty
let any_sub, any_sub' = Ty.cup any Lgl.any_sub, Ty.cup any' Lgl.any_sub'
