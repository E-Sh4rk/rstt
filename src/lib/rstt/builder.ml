(* open Sstt *)

type 'v prim =
| PInt' of int option * int option | PChr' of string | PLgl' of bool
| PLgl | PChr | PInt | PDbl | PClx | PRaw | PAny | PHat of 'v prim | PVar of 'v
| PCup of 'v prim * 'v prim | PCap of 'v prim * 'v prim | PDiff of 'v prim * 'v prim | PNeg of 'v prim

and ('v,'r,'i) t =
| TId of 'i
| TVar of 'v
(* | TRowVar of 'r *)
| TAny | TEmpty | TNull
| TVec of 'v prim
| TVecLen of {len:'v prim ; content:'v prim}
| TVecCstLen of int * 'v prim
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

module TId = struct
  type t = int
  let compare = Stdlib.Int.compare
  let equal = Stdlib.Int.equal
  let pp fmt t = Format.fprintf fmt "%d" t

  let next_id =
    let last = ref 0 in
    fun () ->
      last := !last + 1 ;
      !last
  let create () = next_id ()
end

module TIdMap = Map.Make(TId)
module TIdSet = Set.Make(TId)

(* let rec build env t =
  match t with
  | TId i -> TIdMap.find i env
  | TVar v -> Ty.mk_var v
  | TAny -> Ty.any | TEmpty -> Ty.empty *)
