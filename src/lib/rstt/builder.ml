(* open Sstt *)

module TyExpr = struct
    type prim =
    | TInt' of int option * int option | TChr' of string | TLgl' of bool
    | TLgl | TChr | TInt | TDbl | TClx | TRaw

    and ('v,'r,'i) t =
    | TId of 'i
    | TVar of 'v
    | TRowVar of 'r
    | TAny | TEmpty | TNull
    | TPrim of prim
    (* | TTuple of ('v,'r,'i) t list
    | TRecord of (string * ('v,'r,'i) t) list * ('v,'r,'i) t
    | TCons of ('v,'r,'i) t * ('v,'r,'i) t
    | TArrow of ('v,'r,'i) t * ('v,'r,'i) t *)
    | TCup of ('v,'r,'i) t * ('v,'r,'i) t
    | TCap of ('v,'r,'i) t * ('v,'r,'i) t
    | TDiff of ('v,'r,'i) t * ('v,'r,'i) t
    | TNeg of ('v,'r,'i) t
    (* | TOption of ('v,'r,'i) t *)
    | TWhere of ('v,'r,'i) t * ('i * ('v,'r,'i) t) list
end
