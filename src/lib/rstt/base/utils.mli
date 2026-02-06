open Sstt

val prune_printer_descr : any:Ty.t -> Printer.descr -> Printer.descr
val prune_option_fop : Printer.descr Printer.fop -> Printer.descr Printer.fop
val add_option : Ty.F.t -> Ty.F.t

type interval = int option * int option
val print_interval : string -> int -> Prec.assoc -> Format.formatter -> interval -> unit
