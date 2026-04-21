open Sstt
open Printer
open Prec

module Compare : sig
    open Printer
    val fop : ('d -> 'd -> int) -> 'd fop -> 'd fop -> int
    val builtin : builtin -> builtin -> int
    val descr : descr -> descr -> int
    val op : op -> op -> int
end

val add_printer_param : Printer.params -> unit
val printer_params : unit -> Printer.params
val printer_params' : (Ty.t * string) list -> Printer.params

val print_cup : cmp:('a -> 'a -> int) -> (int -> Prec.assoc -> Format.formatter -> 'a -> unit) ->
    int -> Prec.assoc -> Format.formatter -> 'a list -> unit
val print_cap : cmp:('a -> 'a -> int) -> (int -> Prec.assoc -> Format.formatter -> 'a -> unit) ->
    int -> Prec.assoc -> Format.formatter -> 'a list -> unit
val print_non_empty_dnf : any:string -> cmp:('a -> 'a -> int) ->
    (int -> Prec.assoc -> Format.formatter -> 'a -> unit) ->
    int -> Prec.assoc -> Format.formatter -> ('a list * 'a list) list -> unit
val print_dnf : empty:string -> any:string -> cmp:('a -> 'a -> int) ->
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