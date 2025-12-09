open Sstt
open Printer

module Hat : sig
    val sym : string
    val prec : int
    val assoc : Prec.assoc
    val opinfo : string * int * Prec.assoc
end
module type PrimComp = sig
    val tag_name : string
    val any : Ty.t
    type t
    val any_t : t
    val to_t : build_ctx -> Ty.t -> t option
    val map : (descr -> descr) -> t -> t
    val print : (int -> Prec.assoc -> Format.formatter -> t -> unit)
end
module MakeCompWithNa(P:PrimComp) : sig
    val tag : Tag.t
    val any : Ty.t
    val any' : Ty.t (* Without NA *)
    val mk : Ty.t -> Ty.t
    val mk' : Ty.t -> Ty.t (* Without NA *)
    type 'a t = WithNa of 'a | WithoutNa of 'a | Na
    val destruct : Ty.t -> Ty.t t
    val to_t : build_ctx -> TagComp.t -> P.t t option
    val map : (descr -> descr) -> P.t t -> P.t t
    val print : (int -> Prec.assoc -> Format.formatter -> P.t t -> unit)
end