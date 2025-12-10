%{
open Ast
open Rstt.Builder

let parse_id_or_builtin str =
    match str with
    | "empty" -> TEmpty
    | "any" -> TAny
    | "null" -> TNull
    | str -> TId str

 let parse_builtin_prim str =
    match str with
    | "any" -> PAny
    | "lgl" -> PLgl
    | "chr" -> PChr
    | "int" -> PInt
    | "dbl" -> PDbl
    | "clx" -> PClx
    | "raw" -> PRaw
    | "tt" -> PLgl' true
    | "ff" -> PLgl' false
    | str -> raise (Errors.E_Parser ("Unknown primitive builtin "^str))
%}

%token<string> STRING
%token<Z.t> INT, VLEN
%token<string> ID, VARID, RVARID
%token TYPE WHERE AND
%token BREAK COMMA EQUAL COLON SEMICOLON
%token V HAT ARROW
%token DPOINT QUESTION_MARK
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token LEQ GEQ
%token TOR TAND TNEG TDIFF
%token EOF

%start<program> program
%start<ty> ty_main
%start<command> command

%right ARROW
%left TOR
%left TAND
%left TDIFF
%nonassoc TNEG HAT

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
| hd=simple_ty COMMA tl=separated_nonempty_list(COMMA, simple_ty) { TTuple (hd::tl) }

simple_ty:
| ty=atomic_ty { ty }
| ty1=simple_ty TOR ty2=simple_ty { TCup (ty1, ty2) }
| ty1=simple_ty TDIFF ty2=simple_ty { TDiff (ty1, ty2) }
| ty1=simple_ty TAND ty2=simple_ty { TCap (ty1, ty2) }
| TNEG ty=simple_ty { TNeg (ty) }
| ty1=simple_ty ARROW ty2=simple_ty { TArrow (ty1, ty2) }
// | ty=atomic_ty QUESTION_MARK { TOption (ty) }

atomic_ty:
| id=ID { parse_id_or_builtin id }
| id=VARID { TVar (id) }
| V LPAREN p=prim RPAREN { TVec p }
| V LBRACKET l=prim RBRACKET LPAREN p=prim RPAREN { TVecLen {len=l ; content=p} }
| i=VLEN LPAREN p=prim RPAREN { TVecCstLen (Z.to_int i, p) }
// | id=RVARID { TRowVar (id) }
| LPAREN ty=ty RPAREN { ty }
| LPAREN RPAREN { TTuple [] }

prim:
| id=ID { parse_builtin_prim id }
| id=VARID { PVar (id) }
| p1=prim TOR p2=prim { PCup (p1, p2) }
| p1=prim TDIFF p2=prim { PDiff (p1, p2) }
| p1=prim TAND p2=prim { PCap (p1, p2) }
| TNEG p=prim { PNeg p }
| HAT p=prim { PHat p }
| str=STRING { PChr' str }
| i=INT { let i = Z.to_int i in PInt' (Some i, Some i) }
| LPAREN i1=INT? DPOINT i2=INT? RPAREN
{ let i1,i2 = Option.map Z.to_int i1, Option.map Z.to_int i2 in PInt' (i1,i2) }
