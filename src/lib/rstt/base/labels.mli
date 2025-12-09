open Sstt

type t = Pos of int | Named of string | PosNamed of int * string

val get : t -> Label.t
val pos : int -> Label.t
val named : string -> Label.t
val pos_named : int * string -> Label.t
val info : Label.t -> t
