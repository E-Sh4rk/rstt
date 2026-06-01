open Sstt

type strings = { positive : bool ; content : string list }

val tag : Tag.t
val str : string -> Ty.t
val str' : string -> Ty.t
val var : Var.t -> Ty.t
val var' : Var.t -> Ty.t
val any : Ty.t
val any' : Ty.t
val destruct : Ty.t -> bool * strings
val is_singleton : Ty.t -> bool
