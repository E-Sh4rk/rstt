open Ast
open Rstt
open Rstt_utils

open Output

type res = RBool of bool list | RTy of Ty.t list | RSubst of Subst.t list
let empty_env = empty_env

let is_mono_var v =
  let c = String.get (Var.name v) 1 in
  Char.equal c (Char.lowercase_ascii c)
let is_mono_rvar v =
  let c = String.get (RowVar.name v) 1 in
  Char.equal c (Char.lowercase_ascii c)

let poly_leq t1 t2 =
  let delta = MixVarSet.union (Ty.all_vars t1) (Ty.all_vars t2)
    |> MixVarSet.filter is_mono_var is_mono_rvar in
  Tallying.tally delta [ t1, t2 ] |> List.is_empty |> not

let rec compute_expr env e =
  match e with
  | CTy ty ->
    let ty, env = build_ty env ty in
    RTy [ty], env
  | CSubst s ->
    let s, env = build_subst env s in
    RSubst [s], env
  | CTally cs ->
    let cs, env = build_tally env cs in
    let vars_of_constr (a,b) = MixVarSet.union (Ty.all_vars a) (Ty.all_vars b) in
    let delta = List.fold_left (fun acc c -> MixVarSet.union acc (vars_of_constr c))
      MixVarSet.empty cs
      |> MixVarSet.filter is_mono_var is_mono_rvar in
    RSubst (Tallying.tally delta cs), env
  | CCat (e1, e2) ->
    let r1, env = compute_expr env e1 in
    let r2, env = compute_expr env e2 in
    let r = match r1, r2 with
    | RBool b1, RBool b2 -> RBool (b1@b2)
    | RTy ty1, RTy ty2 -> RTy (ty1@ty2)
    | RSubst s1, RSubst s2 -> RSubst (s1@s2)
    | _, _ -> failwith "Heterogeneous collection."
    in r, env
  | CApp (e1, e2) ->
    let r1, env = compute_expr env e1 in
    let r2, env = compute_expr env e2 in
    let r = match r1, r2 with
    | RTy tys1, RTy tys2 ->
      let apply (ty1, ty2) =
        let arrow = Ty.get_descr ty1 |> Descr.get_arrows in
        Op.Arrows.apply arrow ty2
      in
      RTy (cartesian_product tys1 tys2 |> List.map apply)
    | RSubst s1, RSubst s2 ->
      RSubst (cartesian_product s1 s2 |> List.map (fun (s1, s2) -> Subst.compose s2 s1))
    | RTy ty, RSubst s ->
      RTy (cartesian_product ty s |> List.map (fun (ty, s) -> Subst.apply s ty))
    | _, _ -> failwith "Invalid application."
    in
    r, env
  | CCmp (e1, op, e2) ->
    let r1, env = compute_expr env e1 in
    let r2, env = compute_expr env e2 in
    let tys1, tys2 =
      match r1, r2 with
      | RTy tys1, RTy tys2 -> tys1, tys2
      | _, _ -> failwith "Comparison between non-type values."
    in
    let aux (ty1, ty2) =
      match op with
      | LEQ -> poly_leq ty1 ty2
      | GEQ -> poly_leq ty2 ty1
      | EQ -> poly_leq ty1 ty2 && poly_leq ty2 ty1
    in
    RBool (cartesian_product tys1 tys2 |> List.map aux), env
  | CNorm e ->
    let r, env = compute_expr env e in
    let r =
      match r with
      | RBool bs -> RBool bs
      | RSubst ss -> RSubst ss
      | RTy tys -> RTy (List.map Simplify.partition_vecs tys)
    in
    r, env

let simplify_res e =
  match e with
  | RBool bs -> RBool bs
  | RSubst ss -> RSubst ss
  | RTy tys -> RTy (List.map Transform.simplify tys)

let aliases _env = [] (* TODO *)

let print_res env fmt res =
  match res with
  | RBool bs ->
    let print_bool fmt b = Format.fprintf fmt "%b" b in
    Format.fprintf fmt "%a" (print_seq_space print_bool) bs
  | RTy tys ->
    Format.fprintf fmt "%a"
      (print_seq_cut (Pp.ty' (aliases env))) tys
  | RSubst ss ->
    Format.fprintf fmt "%a"
      (print_seq_cut (Pp.subst' (aliases env))) ss

let treat_elt env elt =
  match elt with
  | DefineAlias (ids, e) ->
    let r, _env = compute_expr env e in
    let r = simplify_res r in
    begin match r with
    | RTy tys when List.length tys = List.length ids ->
      failwith "TODO"
    | _ -> failwith "Definitions must be types." 
    end
  | Expr (str, e) ->
    let r, env = compute_expr env e in
    let r = simplify_res r in
    begin match str with
    | None -> print Msg "@[<v 0>%a@]" (print_res env) r
    | Some str -> print Msg "%s:@[<v 0> %a@]" str (print_res env) r
    end ;
    env
