open Sstt

val prune_printer_descr : any:Ty.t -> Printer.descr -> Printer.descr
val prune_option_fop : Printer.descr Printer.fop -> Printer.descr Printer.fop
val add_option : Ty.F.t -> Ty.F.t
val add_option' : Ty.O.t -> Ty.O.t

type interval = int option * int option
val print_interval : string -> int -> Prec.assoc -> Format.formatter -> interval -> unit

val struct_print : (int -> Prec.assoc -> Format.formatter -> 'a -> unit)
    -> int -> Prec.assoc -> Format.formatter -> 'a -> unit

type 'a prim_line = { pos:bool ; prim:'a list ; pvs:Var.t list ; nvs:Var.t list }
type 'a prim_t = 'a prim_line list
type 'a prim_atom = P of (bool * 'a list) | V of Var.t
val any_prim_t : 'a prim_t
val is_singleton : ('a -> bool) -> 'a prim_t -> bool
val line_to_atoms : 'a prim_line -> 'a prim_atom list * 'a prim_atom list
val t_to_dnf : 'a prim_t -> ('a prim_atom list * 'a prim_atom list) list
