open Sstt

val tt : Ty.t
val ff : Ty.t
val bool : Ty.t
val na : Ty.t
val any : Ty.t
val any_na : Ty.t
val singl : int -> Ty.t
val interval : Utils.interval -> Ty.t
val var : Var.t -> Ty.t
