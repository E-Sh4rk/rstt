open Sstt

type sym = string
type t = Pos of int | Named of string | Sym of sym

val get : t -> Label.t
val pos : int -> Label.t
val named : string -> Label.t
val sym : sym -> Label.t
val info : Label.t -> t
val is_sym : Label.t -> bool

val id : Label.t
(** [id] defines a field used to identify arguments nominally. *)

val npos : Label.t
(** [npos] defines a field used to identify the number of
    positional parameters an argument type expects. *)

val sym_of_ty : Ty.t -> sym list
val substitute : sym -> t -> Ty.t -> Ty.t option
