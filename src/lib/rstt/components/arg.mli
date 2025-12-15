open Sstt

val tag : Tag.t
type 'a t = { pos : 'a list ; pos_named : (string * 'a) list ; tl : 'a ; named : (string * 'a) list }

val any : Ty.t
val mk : Ty.F.t t -> Ty.t
val destruct : Ty.t -> Ty.F.t t
