open Sstt

val mk : Ty.t -> Ty.t
val any : Ty.t
val null : Ty.t
type 'a t = { nullable:bool ; target:'a }
val map : ('a -> 'b) -> 'a t -> 'b t
val destruct : Ty.t -> Ty.t t
