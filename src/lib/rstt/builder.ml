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

(* === Construction of types === *)

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

(* === Resolution of identifiers === *)

module StrMap = Map.Make(String)
type env = {
             tids : TId.t StrMap.t ;
             venv : Var.t StrMap.t ;
             rvenv : RowVar.t StrMap.t ;
             lenv : Label.t StrMap.t
           }

let tvar env str =
  begin match StrMap.find_opt str env.venv with
    | Some v -> v, env
    | None ->
      let v = Var.mk str in
      let venv = StrMap.add str v env.venv in
      let env = { env with venv } in
      v, env
  end

(* let rvar env str =
  begin match StrMap.find_opt str env.rvenv with
    | Some v -> v, env
    | None ->
      let v = RowVar.mk str in
      let rvenv = StrMap.add str v env.rvenv in
      let env = { env with rvenv } in
      v, env
  end *)

let tid env tids str =
  begin match StrMap.find_opt str tids with
    | Some v -> v
    | None -> StrMap.find str env.tids
  end

let resolve_prim env t =
  let env = ref env in
  let rec aux t =
    match t with
    | PAny -> PAny
    | PVar v ->
      let v, env' = tvar !env v in
      env := env' ; PVar v
    | PLgl -> PLgl | PChr -> PChr | PInt -> PInt | PDbl -> PDbl | PClx -> PClx | PRaw -> PRaw
    | PHat t -> PHat (aux t)
    | PCup (t1, t2) -> PCup (aux t1, aux t2)
    | PCap (t1, t2) -> PCap (aux t1, aux t2)
    | PDiff (t1, t2) -> PDiff (aux t1, aux t2)
    | PNeg t -> PNeg (aux t)
    | PInt' (b1,b2) -> PInt' (b1,b2) | PChr' str -> PChr' str | PLgl' b -> PLgl' b
  in
  !env, aux t

let resolve env t =
  let env = ref env in
  let rec aux tids t =
    match t with
    | TId str -> TId (tid !env tids str)
    | TVar v ->
      let v, env' = tvar !env v in
      env := env' ; TVar v
    | TAny -> TAny | TEmpty -> TEmpty | TNull -> TNull
    | TCup (t1,t2) -> TCup (aux tids t1, aux tids t2)
    | TCap (t1,t2) -> TCap (aux tids t1, aux tids t2)
    | TDiff (t1,t2) -> TDiff (aux tids t1, aux tids t2)
    | TNeg t -> TNeg (aux tids t)
    | TVec p ->
      let env', p = resolve_prim !env p in
      env := env' ; TVec p
    | TVecLen { len ; content } ->
      let env', len = resolve_prim !env len in
      let env', content = resolve_prim env' content in
      env := env' ; TVecLen { len ; content }
    | TVecCstLen (i, p) ->
      let env', p = resolve_prim !env p in
      env := env' ; TVecCstLen (i, p)
    | TWhere (t, eqs) ->
      let eqs = eqs |> List.map (fun (x,t) -> x,TId.create (),t) in
      let tids = List.fold_left (fun tids (x,v,_) -> StrMap.add x v tids) tids eqs in
      let t, eqs = aux tids t, List.map (fun (_,v,t) -> v,aux tids t) eqs in
      TWhere (t, eqs)
  in
  !env, aux StrMap.empty t
