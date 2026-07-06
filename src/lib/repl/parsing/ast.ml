
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
type printing_options = { name: string option ; raw: bool }
type elt =
  | DefineAlias of string list * expr
  | Expr of printing_options * expr
type program = elt list
type command = Elt of elt | End

type env = { benv:Builder.env ; aliases:Ty.t Builder.TIdMap.t }
let empty_env = { benv=Builder.empty_env ; aliases=Builder.TIdMap.empty }

let build_ty env t =
  let benv, t = Builder.resolve env.benv t in
  Builder.build env.aliases t, { env with benv }

let build_subst env s =
  let env = ref env in
  let s = s |> List.map (fun (str,ty) ->
      let benv, v = Builder.tvar (!env).benv str in
      let ty, env' = build_ty { !env with benv } ty in
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

let define_alias env name ty =
  let tid = Builder.TId.create_named name in
  let tids = Builder.StrMap.add name tid env.benv.tids in
  let benv = { env.benv with tids } in
  let aliases = Builder.TIdMap.add tid ty env.aliases in
  { benv ; aliases }

let aliases env =
  Builder.TIdMap.bindings env.aliases |> List.map (fun (tid, ty) ->
    let name = Builder.TId.name tid |> Option.get in
    ty, name
  )