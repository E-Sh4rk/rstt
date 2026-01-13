open Sstt

val tag : Tag.t
type 'a atom = { pos : 'a list ; pos_named : (string * 'a) list ; tl : 'a ; named : (string * 'a) list }
type 'a atom' = { pos' : 'a list ; tl' : 'a ; named' : (string * 'a) list }
type 'a elt =
| DefSite of 'a atom
| CallSite of 'a atom'
type 'a t = 'a elt list

val any : Ty.t
val mk : Ty.F.t atom -> Ty.t
val mk' : Ty.F.t atom' -> Ty.t
val destruct : Ty.t -> Ty.F.t t
val map_atom : ('a -> 'b) -> 'a atom -> 'b atom
val map_atom' : ('a -> 'b) -> 'a atom' -> 'b atom'
val map : ('a -> 'b) -> 'a t -> 'b t
