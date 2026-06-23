open Sstt
open Printer
open Prec

module Compare : sig
    open Printer
    val builtin : builtin -> builtin -> int
    val descr : descr -> descr -> int
    val fdescr : fdescr -> fdescr -> int
    val op : op -> op -> int
    val fop : fop -> fop -> int
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

type printing_pos = Tl | Struct | Prim of string (* suffix *)
val current_pos : unit -> printing_pos
val print_descr_ctx' : printing_pos -> int -> assoc -> Format.formatter -> descr -> unit
val print_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit
val print_tl_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit
val print_struct_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit
val print_prim_descr_ctx : int -> assoc -> Format.formatter -> descr -> unit

val print_descr : Format.formatter -> descr -> unit
val print_descr_atomic : Format.formatter -> descr -> unit
val print_field_ctx : int -> assoc -> Format.formatter -> fdescr -> unit
val print : Format.formatter -> descr t -> unit

val pp_struct_tag : (int -> assoc -> Format.formatter -> 'a -> unit) -> int -> assoc -> Format.formatter -> 'a -> unit
val pp_prim_tag : (int -> assoc -> Format.formatter -> 'a -> unit) -> int -> assoc -> Format.formatter -> 'a -> unit

val ty : Format.formatter -> Ty.t -> unit
val ty' : (Ty.t * string) list -> Format.formatter -> Ty.t -> unit
val row : Format.formatter -> Row.t -> unit
val row' : (Ty.t * string) list -> Format.formatter -> Row.t -> unit
val subst : Format.formatter -> Subst.t -> unit
val subst' : (Ty.t * string) list -> Format.formatter -> Subst.t -> unit