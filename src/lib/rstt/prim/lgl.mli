open Sstt

val tag : Tag.t
val tt : Ty.t
val tt' : Ty.t
val ff : Ty.t
val ff' : Ty.t
val bool : bool -> Ty.t
val bool' : bool -> Ty.t
val any : Ty.t
val any' : Ty.t
val any_sub : Ty.t
val any_sub' : Ty.t

val is_singleton : Ty.t -> bool
