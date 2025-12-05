open Sstt

type 'v prim =
| PInt' of int option * int option | PChr' of string | PLgl' of bool
| PLgl | PChr | PInt | PDbl | PClx | PRaw | PAny | PHat of 'v prim | PVar of 'v
| PCup of 'v prim * 'v prim | PCap of 'v prim * 'v prim | PDiff of 'v prim * 'v prim | PNeg of 'v prim

and ('v,'r,'i) t =
| TId of 'i
| TVar of 'v
(* | TRowVar of 'r *)
| TAny | TEmpty | TNull
| TCup of ('v,'r,'i) t * ('v,'r,'i) t
| TCap of ('v,'r,'i) t * ('v,'r,'i) t
| TDiff of ('v,'r,'i) t * ('v,'r,'i) t
| TNeg of ('v,'r,'i) t
| TVec of 'v prim
| TVecLen of {len:'v prim ; content:'v prim}
| TVecCstLen of int * 'v prim
(* | TTuple of ('v,'r,'i) t list
| TRecord of (string * ('v,'r,'i) t) list * ('v,'r,'i) t
| TCons of ('v,'r,'i) t * ('v,'r,'i) t
| TArrow of ('v,'r,'i) t * ('v,'r,'i) t *)
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

let rec build_prim t =
  match t with
  | PAny -> Prim.any
  | PVar v -> Ty.cap Prim.any (Ty.mk_var v)
  | PLgl -> Prim.mk Prim.Lgl.any
  | PChr -> Prim.mk Prim.Chr.any
  | PInt -> Prim.mk Prim.Int.any
  | PDbl -> Prim.mk Prim.Dbl.any
  | PClx -> Prim.mk Prim.Clx.any
  | PRaw -> Prim.mk Prim.Raw.any
  | PHat t -> Ty.cap Prim.any' (build_prim t)
  | PCup (t1, t2) -> Ty.cup (build_prim t1) (build_prim t2)
  | PCap (t1, t2) -> Ty.cap (build_prim t1) (build_prim t2)
  | PDiff (t1, t2) -> Ty.diff (build_prim t1) (build_prim t2)
  | PNeg t -> Ty.diff Prim.any (build_prim t)
  | PInt' (b1,b2) -> Prim.Int.interval (b1,b2) |> Prim.mk
  | PChr' str -> Prim.Chr.str str |> Prim.mk
  | PLgl' b -> Prim.Lgl.bool b |> Prim.mk

let rec build env t =
  match t with
  | TId i -> TIdMap.find i env
  | TVar v -> Ty.mk_var v
  | TAny -> Ty.any | TEmpty -> Ty.empty | TNull -> Null.any
  | TCup (t1,t2) -> Ty.cup (build env t1) (build env t2)
  | TCap (t1,t2) -> Ty.cap (build env t1) (build env t2)
  | TDiff (t1,t2) -> Ty.diff (build env t1) (build env t2)
  | TNeg t -> Ty.neg (build env t)
  | TVec p -> Vec.mk (build_prim p)
  | TVecLen { len ; content } -> Vec.mk ~len:(build_prim len) (build_prim content)
  | TVecCstLen (i, p) -> Vec.mk_len i (build_prim p)
  | TWhere (t, eqs) ->
    let eqs = eqs |> List.map (fun (x,t) -> x,Var.mk "_",t) in
    let env = List.fold_left (fun env (x,v,_) -> TIdMap.add x (Ty.mk_var v) env) env eqs in
    let t, eqs = build env t, List.map (fun (_,v,t) -> v,build env t) eqs in
    let s = Ty.of_eqs eqs |> Subst.of_list1 in
    Subst.apply s t
