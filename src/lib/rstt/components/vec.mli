open Sstt

val tag : Tag.t
val any : Ty.t
val mk : ?len:Ty.t -> Ty.t -> Ty.t
val mk_len : int -> Ty.t -> Ty.t
val length : Ty.t -> Ty.t (* length component is a Prim.Int *)
val content : Ty.t -> Ty.t (* content component is a Prim *)
val partition : Ty.t list
