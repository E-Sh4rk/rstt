open Sstt

val tag : Tag.t
val any : Ty.t
val null : Ty.t
val mk : ?tag:Ty.t -> Ty.t -> Ty.t
(** [mk ~tag target]: an external pointer to [target] whose tag
    ([R_ExternalPtrTag]) has type [tag] ([any], unknown, by default). *)

type 'a t = { nullable:bool ; target:'a ; tag:'a }
val map : ('a -> 'b) -> 'a t -> 'b t
val destruct : Ty.t -> Ty.t t
