open Sstt

type t = Pos of int | Named of string

val get : t -> Label.t
val pos : int -> Label.t
val named : string -> Label.t
val info : Label.t -> t

val id : Label.t
(** [id] defines a field used to identify arguments nominally. *)

val npos : Label.t
(** [npos] defines a field used to identify the number of
    positional parameters an argument type expects. *)
