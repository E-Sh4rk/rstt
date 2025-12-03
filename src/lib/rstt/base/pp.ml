
let pparams = ref Sstt.Printer.empty_params

let add_printer_param p = pparams := Sstt.Printer.merge_params [!pparams ; p]
let printer_params () = !pparams
