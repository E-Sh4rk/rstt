%{
open Ast
open Rstt.Builder

let parse_id_or_builtin str =
    match str with
    | "empty" -> TEmpty
    | "any" -> TAny
    | "null" -> TNull
    | str -> TId str

(* let parse_builtin_prim str =
    match str with
    | "any" -> PAny
    | "lgl" -> PLgl
    | "chr" -> PChr
    | "int" -> PInt
    | "dbl" -> PDbl
    | "clx" -> PClx
    | "raw" -> PRaw
    | str -> raise (Errors.E_Parser ("Unknown primitive builtin "^str)) *)
%}

%token<string> STRING
%token<Z.t> INT
%token<string> ID, VARID, RVARID
%token TYPE WHERE AND
%token BREAK COMMA EQUAL COLON SEMICOLON
%token DPOINT QUESTION_MARK
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token LEQ GEQ
%token TOR TAND TNEG TDIFF
%token EOF

%start<program> program
%start<ty> ty_main
%start<command> command

// %right TARROW
%left TOR
%left TAND
%left TDIFF
%nonassoc TNEG

%%

program:
| p=elt* EOF { p }

command:
| elt=elt { Elt elt }
| EOF { End }

elt:
| TYPE ids=separated_nonempty_list(SEMICOLON, ID) EQUAL e=expr_nocmp BREAK { DefineAlias (ids, e) }
| str=STRING? e=expr BREAK { Expr (str, e) }

expr:
| e=expr_nocmp { e }
| e1=expr_nocmp op=op e2=expr_nocmp { CCmp (e1, op, e2) }

expr_nocmp:
| e=simpl_expr { e }
| e1=expr_nocmp SEMICOLON e2=simpl_expr { CCat (e1, e2) }
| e1=expr_nocmp e2=simpl_expr { CApp (e1, e2) }

simpl_expr:
| s=tsubst { CSubst s }
| t=tally { CTally t }
| ty=ty { CTy ty }
| LBRACKET e=expr_nocmp RBRACKET { e }

op:
| LEQ { LEQ } | EQUAL { EQ } | GEQ { GEQ }

tsubst:
| LBRACKET bindings=separated_list(SEMICOLON, subst_binding) RBRACKET { bindings }

%inline subst_binding:
| v=VARID COLON ty=ty { (v, ty) }

tally:
| LBRACKET cs=separated_nonempty_list(SEMICOLON, tally_binding) RBRACKET { cs }

%inline tally_binding:
| ty1=ty op=op ty2=ty { (ty1, op, ty2) }

ty_main:
| ty=ty EOF { ty }

ty:
| ty=ty_norec { ty }
| ty=ty_norec WHERE defs=separated_nonempty_list(AND, ty_def) { TWhere (ty, defs) }

%inline ty_def: name=ID EQUAL ty=ty_norec { (name, ty) }

ty_norec:
| ty=simple_ty { ty }

simple_ty:
| ty=atomic_ty { ty }
| ty1=simple_ty TOR ty2=simple_ty { TCup (ty1, ty2) }
| ty1=simple_ty TDIFF ty2=simple_ty { TDiff (ty1, ty2) }
| ty1=simple_ty TAND ty2=simple_ty { TCap (ty1, ty2) }
| TNEG ty=simple_ty { TNeg (ty) }
// | ty=atomic_ty QUESTION_MARK { TOption (ty) }

atomic_ty:
| id=ID { parse_id_or_builtin id }
| id=VARID { TVar (id) }
// | id=RVARID { TRowVar (id) }
| LPAREN ty=ty RPAREN { ty }
