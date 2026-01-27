open Sstt

val prune_printer_descr : any:Ty.t -> Printer.descr -> Printer.descr
val prune_option_fop : 'a Printer.fop -> 'a Printer.fop

type interval = int option * int option
val print_interval : string -> int -> Prec.assoc -> Format.formatter -> interval -> unit
