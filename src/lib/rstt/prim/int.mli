open Sstt

val tag : Tag.t
val int : int -> Ty.t
val int' : int -> Ty.t
val var : Var.t -> Ty.t
val interval : Utils.interval -> Ty.t
val interval' : Utils.interval -> Ty.t
val bounded : int * int -> Ty.t
val bounded' : int * int -> Ty.t
val any : Ty.t
val any' : Ty.t
val destruct : Ty.t -> bool * Utils.interval list