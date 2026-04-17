open Sstt

let pparams = ref Printer.empty_params

let add_printer_param p = pparams := Printer.merge_params [!pparams ; p]
let printer_params' aliases = { !pparams with aliases=aliases }
let printer_params () = printer_params' []

let print_cup = Prec.print_cup
let print_non_empty_dnf = Prec.print_non_empty_dnf

let print_descr_ctx = Printer.print_descr_ctx
let print_descr = Printer.print_descr
let print_descr_atomic = Printer.print_descr_atomic
let print_field_ctx = Printer.print_field_ctx
let print = Printer.print

let ty' aliases fmt t =
  let t = Printer.get ~factorize:false (printer_params' aliases) t in
  print fmt t
let ty = ty' []
let subst' aliases fmt s =
  Printer.print_subst (printer_params' aliases) fmt s
let subst = subst' []