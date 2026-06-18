open Sstt

val partition_map3 : ('e -> [< `A of 'a | `B of 'b | `C of 'c]) -> 'e list -> 'a list * 'b list * 'c list

val map_tag_content : (Ty.t -> Ty.t) -> Tag.t -> Descr.t -> Descr.t

val prune_printer_descr : any:Ty.t -> Printer.descr -> Printer.descr
val prune_option_fop : Printer.descr Printer.fop -> Printer.descr Printer.fop
val add_option : Ty.F.t -> Ty.F.t
val add_option' : Ty.O.t -> Ty.O.t

type interval = int option * int option
val print_interval : string -> int -> Prec.assoc -> Format.formatter -> interval -> unit

val struct_print : (int -> Prec.assoc -> Format.formatter -> 'a -> unit)
    -> int -> Prec.assoc -> Format.formatter -> 'a -> unit

type 'a atomic_line = { pos:bool ; prim:'a list ; pvs:Var.t list ; nvs:Var.t list }
type 'a atomic_t = 'a atomic_line list
type 'a atom = P of (bool * 'a list) | V of Var.t
val any_atomic_t : 'a atomic_t
val is_finite : ('a -> bool) -> 'a atomic_t -> bool
val is_singleton : ('a -> bool) -> 'a atomic_t -> bool
val line_to_atoms : 'a atomic_line -> 'a atom list * 'a atom list
val t_to_dnf : 'a atomic_t -> ('a atom list * 'a atom list) list
