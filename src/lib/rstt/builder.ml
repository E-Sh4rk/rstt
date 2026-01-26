open Sstt

type cconst =
| CDouble | CString | CChar | CVoid
| CBool | CTrue | CFalse | CNa | CInt | CIntNa | CIntSingl of int | CIntInterval of int * int

type 'v prim =
| PInt' of int option * int option | PChr' of string | PLgl' of bool
| PLgl | PChr | PInt | PDbl | PClx | PRaw | PAny | PHat of 'v prim | PVar of 'v
| PCup of 'v prim * 'v prim | PCap of 'v prim * 'v prim | PDiff of 'v prim * 'v prim | PNeg of 'v prim

and ('v,'r,'i) t =
| TId of 'i
| TTy of Ty.t
| TVar of 'v
| TRowVar of 'r
| TAny | TEmpty | TNull | TEnv | TSym | TLang
| TCup of ('v,'r,'i) t * ('v,'r,'i) t
| TCap of ('v,'r,'i) t * ('v,'r,'i) t
| TDiff of ('v,'r,'i) t * ('v,'r,'i) t
| TNeg of ('v,'r,'i) t
| TTuple of ('v,'r,'i) t list
| TPrim of 'v prim
| TArrow of ('v,'r,'i) t * ('v,'r,'i) t
| TVec of 'v prim Vec.atom
| TList of ('v,'r,'i) t Lst.atom
| TArg of ('v,'r,'i) t Arg.atom
| TArg' of ('v,'r,'i) t Arg.atom'
| TOption of ('v,'r,'i) t
| TAttr of (('v,'r,'i) t, 'r classes) Attr.atom
| TStruct of ('v,'r,'i) t (* Means that the parameter should not be packed in an Attr container *)
| TCConst of cconst
| TCPtr of ('v,'r,'i) t
| TWhere of ('v,'r,'i) t * ('i * ('v,'r,'i) t) list

and 'r classes =
| CAny | CNoClass
| CClasses of 'r Classes.atom

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

let map_classes f c =
  let aux c =
    let c = match c with
    | CAny | CNoClass -> c
    | CClasses a -> CClasses a
    in
    f c
  in
  aux c

let map f fp fc t =
  let rec aux t =
    let t = match t with
    | TId _ | TTy _ | TVar _ | TRowVar _ | TAny | TEmpty | TNull | TEnv | TSym | TLang -> t
    | TCup (t1, t2) -> TCup (aux t1, aux t2)
    | TCap (t1, t2) -> TCap (aux t1, aux t2)
    | TDiff (t1, t2) -> TDiff (aux t1, aux t2)
    | TNeg t -> TNeg (aux t)
    | TTuple ts -> TTuple (List.map aux ts)
    | TPrim p -> TPrim (fp p)
    | TArrow (t1, t2) -> TArrow (aux t1, aux t2)
    | TVec a -> TVec (Vec.map_atom (map_prim fp) a)
    | TList a -> TList (Lst.map_atom aux a)
    | TArg a -> TArg (Arg.map_atom aux a)
    | TArg' a -> TArg' (Arg.map_atom' aux a)
    | TOption t -> TOption (aux t)
    | TAttr a -> TAttr (Attr.map_atom aux (map_classes fc) a)
    | TStruct t -> TStruct (aux t)
    | TCConst c -> TCConst c
    | TCPtr t -> TCPtr (aux t)
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

let build_cconst t =
  match t with
  | CDouble -> Cenums.double
  | CString -> Cenums.str
  | CChar -> Cenums.char
  | CVoid -> Cenums.void
  | CBool -> Cint.bool
  | CTrue -> Cint.tt
  | CFalse -> Cint.ff
  | CNa -> Cint.na
  | CInt -> Cint.any
  | CIntNa -> Cint.any_na
  | CIntSingl i -> Cint.singl i
  | CIntInterval (i1,i2) -> Cint.interval (i1,i2)

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

let build_classes t =
  match t with
  | CAny -> Classes.any
  | CNoClass -> Classes.noclass
  | CClasses a -> Classes.mk a

let rec build_struct env t =
  match t with
  | TId i -> (try TIdMap.find i env with Not_found ->
    invalid_arg ("type of "^(string_of_int i)^" not found in the environment"))
  | TTy ty -> ty
  | TVar v -> Ty.mk_var v
  | TRowVar _ -> invalid_arg "Unexpected row variable"
  | TAny -> Ty.any | TEmpty -> Ty.empty
  | TCup (t1,t2) -> Ty.cup (build_struct env t1) (build_struct env t2)
  | TCap (t1,t2) -> Ty.cap (build_struct env t1) (build_struct env t2)
  | TDiff (t1,t2) -> Ty.diff (build_struct env t1) (build_struct env t2)
  | TNeg t -> Ty.neg (build_struct env t)
  | TNull -> Null.any | TEnv -> Env.any | TSym -> Sym.any | TLang -> Lang.any
  | TTuple lst -> Descr.mk_tuple (List.map (build env) lst) |> Ty.mk_descr
  | TPrim p -> build_prim p
  | TArrow (t1,t2) -> Descr.mk_arrow (build env t1, build env t2) |> Ty.mk_descr
  | TVec a -> Vec.map_atom build_prim a |> Vec.mk
  | TList a -> Lst.map_atom (build_field env) a |> Lst.mk
  | TArg a -> Arg.map_atom (build_field env) a |> Arg.mk
  | TArg' a -> Arg.map_atom' (build_field env) a |> Arg.mk'
  | TCConst c -> build_cconst c
  | TCPtr t -> Cptr.mk (build env t)
  | TOption _ -> invalid_arg "Unexpected optional type"
  | TAttr _ -> invalid_arg "Unexpected attributes"
  | TStruct _ -> invalid_arg "Unexpected struct"
  | TWhere _ -> invalid_arg "Unexpected where clause"

and build env t =
  match t with
  | TId i -> (try TIdMap.find i env with Not_found ->
    invalid_arg ("type of "^(string_of_int i)^" not found in the environment"))
  | TTy ty -> ty
  | TAny -> Ty.any | TEmpty -> Ty.empty
  | TVar v -> Ty.mk_var v
  | TRowVar _ -> invalid_arg "Unexpected row variable"
  | TCup (t1,t2) -> Ty.cup (build env t1) (build env t2)
  | TCap (t1,t2) -> Ty.cap (build env t1) (build env t2)
  | TDiff (t1,t2) -> Ty.diff (build env t1) (build env t2)
  | TNeg t -> Ty.neg (build env t)
  | TWhere (t, eqs) ->
    let eqs = eqs |> List.map (fun (x,t) -> x,Var.mk "_",t) in
    let env = List.fold_left (fun env (x,v,_) -> TIdMap.add x (Ty.mk_var v) env) env eqs in
    let t, eqs = build env t, List.map (fun (_,v,t) -> v,build env t) eqs in
    let s = Ty.of_eqs eqs |> Subst.of_list1 in
    Subst.apply s t
  (* Explicit attr *)
  | TAttr a -> Attr.map_atom (build_struct env) build_classes a |> Attr.mk
  | TStruct t -> build_struct env t
  (* We don't need attributes for C values, primitive types, tuples, and args *)
  | TPrim p -> build_prim p
  | TCConst c -> build_cconst c
  | TCPtr t -> Cptr.mk (build env t)
  | TTuple lst -> Descr.mk_tuple (List.map (build env) lst) |> Ty.mk_descr
  | TArg a -> Arg.map_atom (build_field env) a |> Arg.mk
  | TArg' a -> Arg.map_atom' (build_field env) a |> Arg.mk'
  (* R types *)
  | t -> Attr.mk {content=build_struct env t ; classes=Classes.any}

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
    | None -> (try StrMap.find str env.tids with Not_found ->
      invalid_arg ("type of "^str^" not found in the environment"))  
  end

let resolve_prim env t =
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
  aux t

let resolve_classes env t =
  let aux t =
    match t with
    | CAny -> CAny | CNoClass -> CNoClass
    | CClasses a ->
      let rvar v =
        let env', v = rvar !env v in
        env := env' ; v
      in
      CClasses (Classes.map_atom rvar a)
  in
  aux t

let resolve env t =
  let rec aux tids t =
    match t with
    | TId str -> TId (tid !env tids str)
    | TTy ty -> TTy ty
    | TVar v ->
      let env', v = tvar !env v in
      env := env' ; TVar v
    | TRowVar v ->
      let env', v = rvar !env v in
      env := env' ; TRowVar v
    | TAny -> TAny | TEmpty -> TEmpty | TNull -> TNull | TEnv -> TEnv
    | TSym -> TSym | TLang -> TLang
    | TCup (t1,t2) -> TCup (aux tids t1, aux tids t2)
    | TCap (t1,t2) -> TCap (aux tids t1, aux tids t2)
    | TDiff (t1,t2) -> TDiff (aux tids t1, aux tids t2)
    | TNeg t -> TNeg (aux tids t)
    | TTuple lst -> TTuple (List.map (aux tids) lst)
    | TPrim p -> TPrim (resolve_prim env p)
    | TArrow (t1,t2) -> TArrow (aux tids t1, aux tids t2)
    | TVec a -> TVec (Vec.map_atom (resolve_prim env) a)
    | TList a -> TList (Lst.map_atom (aux tids) a)
    | TArg a -> TArg (Arg.map_atom (aux tids) a)
    | TArg' a -> TArg' (Arg.map_atom' (aux tids) a)
    | TOption t -> TOption (aux tids t)
    | TAttr a -> TAttr (Attr.map_atom (aux tids) (resolve_classes env) a)
    | TStruct t -> TStruct (aux tids t)
    | TCConst c -> TCConst c
    | TCPtr t -> TCPtr (aux tids t)
    | TWhere (t, eqs) ->
      let eqs = eqs |> List.map (fun (x,t) -> x,TId.create (),t) in
      let tids = List.fold_left (fun tids (x,v,_) -> StrMap.add x v tids) tids eqs in
      let t, eqs = aux tids t, List.map (fun (_,v,t) -> v,aux tids t) eqs in
      TWhere (t, eqs)
  in
  aux StrMap.empty t

let resolve_prim env p =
  let env = ref env in
  let p = resolve_prim env p in
  !env, p
let resolve_classes env p =
  let env = ref env in
  let p = resolve_classes env p in
  !env, p
let resolve env p =
  let env = ref env in
  let p = resolve env p in
  !env, p
