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

let map_prim f p =
  let rec aux p =
    let p = match p with
    | PInt' _ | PChr' _ | PLgl' _
    | PLgl | PChr | PInt | PDbl | PClx | PRaw | PAny | PVar _ -> p
    | PHat p -> PHat (aux p)
    | PCup (p1, p2) -> PCup (aux p1, aux p2)
    | PCap (p1, p2) -> PCap (aux p1, aux p2)
    | PDiff (p1, p2) -> PDiff (aux p1, aux p2)
    | PNeg p -> PNeg (aux p)
    in
    f p
  in
  aux p

let map f fp t =
  let rec aux t =
    let t = match t with
    | TId _ | TVar _ | TRowVar _ | TAny | TEmpty | TNull -> t
    | TCup (t1, t2) -> TCup (aux t1, aux t2)
    | TCap (t1, t2) -> TCap (aux t1, aux t2)
    | TDiff (t1, t2) -> TDiff (aux t1, aux t2)
    | TNeg t -> TNeg (aux t)
    | TTuple ts -> TTuple (List.map aux ts)
    | TPrim p -> TPrim (fp p)
    | TArrow (t1, t2) -> TArrow (aux t1, aux t2)
    | TVec p -> TVec (map_prim fp p)
    | TVecLen { len ; content } -> TVecLen { len ; content=map_prim fp content }
    | TVecCstLen (i, p) -> TVecCstLen (i, map_prim fp p)
    | TList (pos,named,tl) ->
      TList (List.map aux pos, List.map (fun (str,t) -> str, aux t) named, aux tl)
    | TOption t -> TOption (aux t)
    | TWhere (t, lst) -> TWhere (aux t, lst |> List.map (fun (id, t) -> id, aux t))
    in
    f t
  in
  aux t

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
  | TRowVar _ -> invalid_arg "Unexpected row variable"
  | TAny -> Ty.any | TEmpty -> Ty.empty | TNull -> Null.any
  | TCup (t1,t2) -> Ty.cup (build env t1) (build env t2)
  | TCap (t1,t2) -> Ty.cap (build env t1) (build env t2)
  | TDiff (t1,t2) -> Ty.diff (build env t1) (build env t2)
  | TNeg t -> Ty.neg (build env t)
  | TTuple lst -> Descr.mk_tuple (List.map (build env) lst) |> Ty.mk_descr
  | TPrim p -> build_prim p
  | TArrow (t1,t2) -> Descr.mk_arrow (build env t1, build env t2) |> Ty.mk_descr
  | TVec p -> Vec.mk (build_prim p)
  | TVecLen { len ; content } -> Vec.mk ~len:(build_prim len) (build_prim content)
  | TVecCstLen (i, p) -> Vec.mk_len i (build_prim p)
  | TList (pos, named, tl) ->
    let pos = List.map (build_field env) pos in
    let named = List.map (fun (str, t) -> str, build_field env t) named in
    let tl = build_field env tl in
    Lst.mk pos named tl
  | TOption _ -> invalid_arg "Unexpected optional type"
  | TWhere (t, eqs) ->
    let eqs = eqs |> List.map (fun (x,t) -> x,Var.mk "_",t) in
    let env = List.fold_left (fun env (x,v,_) -> TIdMap.add x (Ty.mk_var v) env) env eqs in
    let t, eqs = build env t, List.map (fun (_,v,t) -> v,build env t) eqs in
    let s = Ty.of_eqs eqs |> Subst.of_list1 in
    Subst.apply s t

and build_field env t =
  match t with
  | TOption t -> Ty.F.mk_descr (build env t |> Ty.O.optional)
  | TRowVar v -> Ty.F.mk_var v
  | TCup (t1,t2) ->
      let t1 = build_field env t1 in
      let t2 = build_field env t2 in
      Ty.F.cup t1 t2
  | TCap (t1,t2) ->
      let t1 = build_field env t1 in
      let t2 = build_field env t2 in
      Ty.F.cap t1 t2
  | TDiff (t1,t2) ->
      let t1 = build_field env t1 in
      let t2 = build_field env t2 in
      Ty.F.diff t1 t2
  | TNeg t -> Ty.F.neg (build_field env t)
  | t -> Ty.F.mk_descr (build env t |> Ty.O.required)

(* === Resolution of identifiers === *)

module StrMap = Map.Make(String)
type env = {
             tids : TId.t StrMap.t ;
             venv : Var.t StrMap.t ;
             rvenv : RowVar.t StrMap.t ;
             lenv : Label.t StrMap.t
           }
let empty_env = { tids=StrMap.empty ; venv=StrMap.empty ; rvenv=StrMap.empty ; lenv=StrMap.empty }

let tvar env str =
  begin match StrMap.find_opt str env.venv with
    | Some v -> env, v
    | None ->
      let v = Var.mk str in
      let venv = StrMap.add str v env.venv in
      let env = { env with venv } in
      env, v
  end

let rvar env str =
  begin match StrMap.find_opt str env.rvenv with
    | Some v -> env, v
    | None ->
      let v = RowVar.mk str in
      let rvenv = StrMap.add str v env.rvenv in
      let env = { env with rvenv } in
      env, v
  end

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
      let env', v = tvar !env v in
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
      let env', v = tvar !env v in
      env := env' ; TVar v
    | TRowVar v ->
      let env', v = rvar !env v in
      env := env' ; TRowVar v
    | TAny -> TAny | TEmpty -> TEmpty | TNull -> TNull
    | TCup (t1,t2) -> TCup (aux tids t1, aux tids t2)
    | TCap (t1,t2) -> TCap (aux tids t1, aux tids t2)
    | TDiff (t1,t2) -> TDiff (aux tids t1, aux tids t2)
    | TNeg t -> TNeg (aux tids t)
    | TTuple lst -> TTuple (List.map (aux tids) lst)
    | TPrim p ->
      let env', p = resolve_prim !env p in
      env := env' ; TPrim p
    | TArrow (t1,t2) -> TArrow (aux tids t1, aux tids t2)
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
    | TList (pos, named, tl) ->
      let pos = List.map (aux tids) pos in
      let named = List.map (fun (str,t) -> str, aux tids t) named in
      let tl = aux tids tl in
      TList (pos, named, tl)
    | TOption t -> TOption (aux tids t)
    | TWhere (t, eqs) ->
      let eqs = eqs |> List.map (fun (x,t) -> x,TId.create (),t) in
      let tids = List.fold_left (fun tids (x,v,_) -> StrMap.add x v tids) tids eqs in
      let t, eqs = aux tids t, List.map (fun (_,v,t) -> v,aux tids t) eqs in
      TWhere (t, eqs)
  in
  !env, aux StrMap.empty t
