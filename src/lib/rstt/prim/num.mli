open Sstt

(* Num is not a real component ; it is a transient component
   used for pretty-printing both the int and dbl components at the same time. *)

val tag : Tag.t
val of_dbl : Ty.t -> Ty.t option
val of_int : Ty.t -> Ty.t option

(* Constructors *)
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
