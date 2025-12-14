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
  | CNorm of expr
type elt =
  | DefineAlias of string list * expr
  | Expr of string option * expr
type program = elt list
type command = Elt of elt | End

type env

val empty_env : env
val build_ty : env -> ty -> Ty.t * env
val build_subst : env -> subst -> Subst.t * env
val build_tally : env -> tally -> Tallying.constr list * env
