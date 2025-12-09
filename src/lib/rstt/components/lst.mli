open Sstt

val tag : Tag.t
val any : Ty.t
val mk : Ty.F.t list -> (string * Ty.F.t) list -> Ty.F.t -> Ty.t

type 'a atom = 'a list * (string * 'a) list * 'a
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

val destruct : Ty.t -> Ty.F.t t
