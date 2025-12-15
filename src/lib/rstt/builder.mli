open Sstt

type 'v prim =
| PInt' of int option * int option | PChr' of string | PLgl' of bool
| PLgl | PChr | PInt | PDbl | PClx | PRaw | PAny | PHat of 'v prim | PVar of 'v
| PCup of 'v prim * 'v prim | PCap of 'v prim * 'v prim | PDiff of 'v prim * 'v prim | PNeg of 'v prim

and ('v,'r,'i) t =
| TId of 'i
| TVar of 'v
| TRowVar of 'r
| TAny | TEmpty | TNull
| TCup of ('v,'r,'i) t * ('v,'r,'i) t
| TCap of ('v,'r,'i) t * ('v,'r,'i) t
| TDiff of ('v,'r,'i) t * ('v,'r,'i) t
| TNeg of ('v,'r,'i) t
| TTuple of ('v,'r,'i) t list
| TPrim of 'v prim
| TArrow of ('v,'r,'i) t * ('v,'r,'i) t
| TVec of 'v prim
| TVecLen of {len:'v prim ; content:'v prim}
| TVecCstLen of int * 'v prim
| TList of (('v,'r,'i) t list) * (string * ('v,'r,'i) t) list * ('v,'r,'i) t
| TOption of ('v,'r,'i) t
| TWhere of ('v,'r,'i) t * ('i * ('v,'r,'i) t) list

val map_prim : ('v prim -> 'v prim)
  -> 'v prim -> 'v prim
val map : (('v,'r,'i) t -> ('v,'r,'i) t)
  -> ('v prim -> 'v prim)
  -> ('v,'r,'i) t -> ('v,'r,'i) t

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
val build : Ty.t TIdMap.t -> (Var.t,RowVar.t,TId.t) t -> Ty.t
val build_field : Ty.t TIdMap.t -> (Var.t,RowVar.t,TId.t) t -> Ty.F.t

module StrMap : Map.S with type key=string
type env = {
             tids : TId.t StrMap.t ;
             venv : Var.t StrMap.t ;
             rvenv : RowVar.t StrMap.t ;
             lenv : Label.t StrMap.t
           }
val empty_env : env
val tvar : env -> string -> env * Var.t
val rvar : env -> string -> env * RowVar.t
val resolve_prim : env -> string prim -> env * Var.t prim
val resolve : env -> (string,string,string) t -> env * (Var.t,RowVar.t,TId.t) t