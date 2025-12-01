open Ast
open Rstt

val empty_env : env
val treat_elt : ?pparams:Printer.params -> env -> elt -> env
