
open Rstt

type op = LEQ | EQ | GEQ
type ty = (string,string,string) Builder.t
type subst = (string * ty) list
type tally = (ty * op * ty) list
type expr =
  | CTy of ty
  | CSubst of subst
  | CTally of tally
  | CCat of expr * expr
  | CApp of expr * expr
  | CCmp of expr * op * expr
type elt =
  | DefineAlias of string list * expr
  | Expr of string option * expr
type program = elt list
type command = Elt of elt | End

type env = Builder.env
let empty_env = Builder.empty_env

let build_ty env t =
  let env, t = Builder.resolve env t in
  Builder.build Builder.TIdMap.empty t, env

let build_subst env s =
  let env = ref env in
  let s = s |> List.map (fun (str,ty) ->
      let env', v = Builder.tvar !env str in
      let ty, env' = build_ty env' ty in
      env := env' ; (v, ty)
    ) in
  let s = Subst.of_list1 s in
  s, !env

let build_tally env cs =
  let env = ref env in
  let cs = cs |> List.concat_map (fun (ty1,op,ty2) ->
      let ty1, env' = build_ty !env ty1 in
      let ty2, env' = build_ty env' ty2 in
      env := env' ;
      match op with
      | LEQ -> [ty1,ty2]
      | GEQ -> [ty2,ty1]
      | EQ -> [ty1,ty2 ; ty2,ty1]
    )
  in
  cs, !env
