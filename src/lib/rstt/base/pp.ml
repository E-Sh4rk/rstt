open Sstt

let pparams = ref Printer.empty_params

let add_printer_param p = pparams := Printer.merge_params [!pparams ; p]
let printer_params' aliases = { !pparams with aliases=aliases }
let printer_params () = printer_params' []

let ty' aliases fmt t =
  let t = Printer.get ~inline:true (printer_params' aliases) t in
  Printer.print fmt t
let ty = ty' []
let subst' aliases fmt s =
  Printer.print_subst (printer_params' aliases) fmt s
let subst = subst' []