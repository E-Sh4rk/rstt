open Sstt

(* Num is not a real component ; it is a transient component
   used for pretty-printing both the int and dbl components at the same time. *)

val tag : Tag.t
val any : Ty.t
val any' : Ty.t

val of_dbl : Ty.t -> Ty.t option
val of_int : Ty.t -> Ty.t option
