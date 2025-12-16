open Sstt

val tag : Tag.t
val any : Ty.t

type 'a atom = 'a list * (string * 'a) list * 'a
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

val mk : Ty.F.t atom -> Ty.t

val destruct : Ty.t -> Ty.F.t t
val map_atom : ('a -> 'b) -> 'a atom -> 'b atom
val map_line : ('a -> 'b) -> 'a line -> 'b line
val map : ('a -> 'b) -> 'a t -> 'b t