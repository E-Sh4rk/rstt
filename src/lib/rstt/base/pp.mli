open Sstt
open Printer
open Prec

val add_printer_param : Printer.params -> unit
val printer_params : unit -> Printer.params
val printer_params' : (Ty.t * string) list -> Printer.params

val print_cup : (int -> Prec.assoc -> Format.formatter -> 'a -> unit) ->
    int -> Prec.assoc -> Format.formatter -> 'a list -> unit
val print_non_empty_dnf : any:string ->
    (int -> Prec.assoc -> Format.formatter -> 'a -> unit) ->
    int -> Prec.assoc -> Format.formatter -> ('a list * 'a list) list -> unit

val print_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit
val print_descr : Format.formatter -> descr -> unit
val print_descr_atomic : Format.formatter -> descr -> unit
val print_field_ctx : int -> assoc -> Format.formatter -> descr fop -> unit
val print : Format.formatter -> descr t -> unit

val ty : Format.formatter -> Ty.t -> unit
val ty' : (Ty.t * string) list -> Format.formatter -> Ty.t -> unit
val subst : Format.formatter -> Subst.t -> unit
val subst' : (Ty.t * string) list -> Format.formatter -> Subst.t -> unit