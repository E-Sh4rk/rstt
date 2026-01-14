open Sstt

type 'a atom =
  | AnyLength of 'a
  | CstLength of int * 'a
  | VarLength of 'a (* subtype of Prim.Int.any *) * 'a
type 'a line = 'a atom * 'a atom list
type 'a t = 'a line list

val tag : Tag.t
val any : Ty.t
val mk : Ty.t atom -> Ty.t
val partition : Ty.t list

val destruct : Ty.t -> Ty.t t
val map_atom : ('a -> 'b) -> 'a atom -> 'b atom
val map_line : ('a -> 'b) -> 'a line -> 'b line
val map : ('a -> 'b) -> 'a t -> 'b t
