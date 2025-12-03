open Sstt

type interval = int option * int option

val int : int -> Ty.t
val interval : interval -> Ty.t
val bounded : int * int -> Ty.t
val any : Ty.t
