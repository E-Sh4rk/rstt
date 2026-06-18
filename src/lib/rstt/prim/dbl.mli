open Sstt

val tag : Tag.t
val int : int -> Ty.t
val int' : int -> Ty.t
val var : Var.t -> Ty.t
val var' : Var.t -> Ty.t
val interval : Utils.interval -> Ty.t
val interval' : Utils.interval -> Ty.t
val bounded : int * int -> Ty.t
val bounded' : int * int -> Ty.t
val any : Ty.t
val any' : Ty.t
val any_sub : Ty.t
val any_sub' : Ty.t

type t = { integers:Utils.interval Utils.atomic_t ; neg:bool }
val destruct : Ty.t -> bool (* na? *) * t
val is_singleton : Ty.t -> bool
