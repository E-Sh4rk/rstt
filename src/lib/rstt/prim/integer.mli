open Sstt

val tag : Tag.t
val int : int -> Ty.t
val var : Var.t -> Ty.t
val interval : Utils.interval -> Ty.t
val bounded : int * int -> Ty.t
val any : Ty.t

type t = Utils.interval Utils.atomic_t
val any_t : t
val may_not_feature_any : t -> bool
val to_t : ?pos:bool -> Ty.t -> t option
val print : any:string -> var:string -> pp_int:(Format.formatter -> int -> unit)
    -> int -> Prec.assoc -> Format.formatter -> t -> unit

val destruct : ?pos:bool -> Ty.t -> t
val is_singleton : Ty.t -> bool
