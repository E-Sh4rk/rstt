open Sstt

val add_printer_param : Printer.params -> unit
val printer_params : unit -> Printer.params
val printer_params' : (Ty.t * string) list -> Printer.params
val ty : Format.formatter -> Ty.t -> unit
val ty' : (Ty.t * string) list -> Format.formatter -> Ty.t -> unit