open Sstt

type t = Pos of int | Named of string | Sym of t list

val name : t -> string
val of_name : string -> t
val get : t -> Label.t
val pos : int -> Label.t
val named : string -> Label.t
val sym : t list -> Label.t
val info : Label.t -> t
val is_sym : Label.t -> bool

val id : Label.t
(** [id] defines a field used to identify arguments nominally. *)

val npos : Label.t
(** [npos] defines a field used to identify the number of
    positional parameters an argument type expects. *)

val sym_of_ty : Ty.t -> t list
type sym_subst = { sym:t ; target:t }
val substitute : sym_subst list -> Ty.t -> Ty.t
