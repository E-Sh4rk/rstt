open Sstt

type 'a atom = { content:'a ; classes:'a option }
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

val mk : Ty.t atom -> Ty.t
val mk_noclass : Ty.t -> Ty.t
val mk_anyclass : Ty.t -> Ty.t
val any : Ty.t
val destruct : Ty.t -> Ty.t t
