open Sstt

val tag : Tag.t
val any : Ty.t

type ('l,'a) atom = { bindings: ('l * 'a) list ; tl:'a }
type ('l,'a) line = ('l,'a) atom list * ('l,'a) atom list
type ('l,'a) t = ('l,'a) line list

val mk : (string, Ty.F.t) atom -> Ty.t
val empty : Ty.t

val destruct : Ty.t -> (string, Ty.F.t) t

val proj : string -> Ty.t -> Ty.F.t
(** [proj lbl ty] over-approximates [ty] by a single atom
    and returns the (possibly absent) type of its field [lbl]. *)

val map_atom : ('l -> 'm) -> ('a -> 'b) -> ('l,'a) atom -> ('m,'b) atom
val map_line : ('a -> 'b) -> ('l,'a) line -> ('l,'b) line
val map : ('a -> 'b) -> ('l,'a) t -> ('l,'b) t