open Sstt

type interval = int option * int option

val tag : Tag.t
val int : int -> Ty.t
val int' : int -> Ty.t
val var : Var.t -> Ty.t
val interval : interval -> Ty.t
val interval' : interval -> Ty.t
val bounded : int * int -> Ty.t
val bounded' : int * int -> Ty.t
val any : Ty.t
val any' : Ty.t
val destruct : Ty.t -> bool * interval list