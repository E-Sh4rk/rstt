open Sstt

type 'v prim =
| PInt' of int option * int option | PChr' of string | PLgl' of bool
| PLgl | PChr | PInt | PDbl | PClx | PRaw | PAny | PHat of 'v prim | PVar of 'v
| PCup of 'v prim * 'v prim | PCap of 'v prim * 'v prim | PDiff of 'v prim * 'v prim | PNeg of 'v prim

and ('v,'r,'i) t =
| TId of 'i
| TVar of 'v
| TAny | TEmpty | TNull
| TCup of ('v,'r,'i) t * ('v,'r,'i) t
| TCap of ('v,'r,'i) t * ('v,'r,'i) t
| TDiff of ('v,'r,'i) t * ('v,'r,'i) t
| TNeg of ('v,'r,'i) t
| TVec of 'v prim
| TVecLen of {len:'v prim ; content:'v prim}
| TVecCstLen of int * 'v prim
| TWhere of ('v,'r,'i) t * ('i * ('v,'r,'i) t) list

module TId : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : unit -> t
end

module TIdMap : Map.S with type key=TId.t
module TIdSet : Set.S with type elt=TId.t

val build_prim : Var.t prim -> Ty.t
val build : Ty.t TIdMap.t -> (Var.t,'r,TId.t) t -> Ty.t
