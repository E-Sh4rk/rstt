open Sstt

val tag : Tag.t
val str : string -> Ty.t
val str' : string -> Ty.t
val var : Var.t -> Ty.t
val var' : Var.t -> Ty.t
val any : Ty.t
val any' : Ty.t
val any_sub : Ty.t
val any_sub' : Ty.t

val destruct : Ty.t -> bool (* na? *) * string Utils.prim_t
val is_singleton : Ty.t -> bool
