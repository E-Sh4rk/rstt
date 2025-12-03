open Sstt

type interval = char * char

val chr : char -> Ty.t
val interval : interval -> Ty.t
val any : Ty.t
