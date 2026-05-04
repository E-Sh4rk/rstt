open Sstt

val mk_nonstring : Ty.t -> Ty.t
val singl_string : string -> Ty.t
val var_string : Var.t -> Ty.t
val any : Ty.t
val string : Ty.t
val null : Ty.t
type 'a t = { nullable:bool ; target:'a ; str:'a }
val map : ('a -> 'b) -> 'a t -> 'b t
val destruct : Ty.t -> Ty.t t
