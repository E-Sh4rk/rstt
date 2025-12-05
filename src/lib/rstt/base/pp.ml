open Sstt

let pparams = ref Printer.empty_params

let add_printer_param p = pparams := Printer.merge_params [!pparams ; p]
let printer_params' aliases = { !pparams with aliases=aliases }
let printer_params () = printer_params' []

let ty' alias fmt t =
  let t = Printer.get ~inline:true (printer_params' alias) t in
  Printer.print fmt t
let ty = ty' []