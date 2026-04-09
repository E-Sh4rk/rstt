%{
open Ast
open Rstt.Builder
open Rstt

let parse_id_or_builtin str =
    match str with
    | "empty" -> TEmpty
    | "any" -> TAny
    | "null" -> TNull
    | "env" -> TEnv
    | "sym" -> TSym
    | "lang" -> TLang
    | "list" -> TList{pos=[];named=[];sym=[];tl=TOption TAny}
    (* C stuff  *)
    | "c_double" -> TCConst CDouble
    | "c_string" -> TCConst CString
    | "c_char" -> TCConst CChar
    | "c_void" -> TCConst CVoid
    | "c_int_na" -> TCConst CIntNa
    | "c_int" -> TCConst CInt
    | "c_na" -> TCConst CNa
    | "c_bool" -> TCConst CBool
    | "c_true" -> TCConst CTrue
    | "c_false" -> TCConst CFalse
    | str -> TId str

 let parse_builtin_prim str =
    match str with
    | "any" -> PAny
    | "vec" -> PAny
    | "lgl" -> PLgl
    | "chr" -> PChr
    | "int" -> PInt
    | "dbl" -> PDbl
    | "clx" -> PClx
    | "raw" -> PRaw
    | str -> raise (Errors.E_Parser ("Unknown primitive builtin "^str))

 type field = Pos of ty | Named of string * ty | Sym of string * ty
 let split_fields lst =
    let rec pos_fields lst =
        match lst with
        | (Pos t)::lst ->
            let ts, lst = pos_fields lst in
            (t::ts), lst
        | _ -> [], lst
    in
    let pos, lst = pos_fields lst in
    let named, sym = lst |> List.partition_map (function
        | Named (str,t) -> Either.left (str,t)
        | Sym (str,t) -> Either.right (Labels.sym_of_name str,t)
        | _ -> raise (Errors.E_Parser ("Unexpected positional field"))
    ) in
    pos, named, sym
%}

%token<string> STRING, SHORT, SBRACKET
%token<Z.t> INT, VLEN
%token<string> ID, VARID, RVARID, SYMID
%token<string*Z.t> SLEN
%token TYPE
%token BREAK COMMA EQUAL COLON SEMICOLON ELLIPSIS
%token C VP VB P T S HAT ARROW STAR
%token PI PC PCI PCS
%token TT FF
%token QUESTION_MARK EXCL_MARK DPOINT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET ALPAREN
%token LEQ GEQ LT GT
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
| str=name e=expr BREAK { Expr (str, e) }

%inline name:
| { None }
| str=STRING COLON { Some str }

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
| EXCL_MARK LBRACKET e=expr_nocmp RBRACKET { CNorm e }
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

classes:
| LT classes=separated_list(COMMA, ID) tl=classes_tail GT
{ { pos=List.map (fun str -> Classes.L (str, [])) classes ; neg=[] ; unk=[] ; tail=tl } }

%inline classes_tail:
| { Classes.NoOther } | STAR { Classes.AllOthers } | ELLIPSIS { Classes.Unknown }
| SEMICOLON id=RVARID { Classes.RowVars ([[id],[]]) }

ty:
| ty=simple_ty { ty }

simple_ty:
| ty=atomic_ty classes=classes { TAttr {content=ty;classes=CClasses classes} }
| classes=classes { TAttr {content=TAny;classes= CClasses classes} }
| ty=atomic_ty { ty }
| ty1=simple_ty TOR ty2=simple_ty { TCup (ty1, ty2) }
| ty1=simple_ty TDIFF ty2=simple_ty { TDiff (ty1, ty2) }
| ty1=simple_ty TAND ty2=simple_ty { TCap (ty1, ty2) }
| TNEG ty=simple_ty { TNeg (ty) }
| ty1=simple_ty ARROW ty2=simple_ty { TArrow (ty1, ty2) }
| ty=atomic_ty QUESTION_MARK { TOption (ty) }

atomic_ty:
| id=ID { parse_id_or_builtin id }
| id=VARID { TVar (id) }
| id=RVARID { TRowVar (id) }
| id=SYMID { TSymLabel (id) }
| LPAREN ty=ty RPAREN { ty }
| P p=prim RPAREN { TPrim p }
| S s=ty RPAREN { TStruct s }
(* Vectors *)
| VP p=prim RPAREN { TVec (AnyLength p) }
| s=SHORT { TVec (AnyLength (parse_builtin_prim s)) }
| HAT s=SHORT { TVec (AnyLength (PHat (parse_builtin_prim s))) }
| VB l=prim RBRACKET LPAREN p=prim RPAREN { TVec (VarLength (l,p)) }
| s=SBRACKET l=prim RBRACKET {TVec (VarLength (l,parse_builtin_prim s)) }
| HAT s=SBRACKET l=prim RBRACKET { TVec (VarLength (l,PHat (parse_builtin_prim s))) }
| i=VLEN LPAREN p=prim RPAREN { TVec (CstLength (Z.to_int i, p)) }
| s=SLEN { let (s,i) = s in TVec (CstLength (Z.to_int i, parse_builtin_prim s)) }
| HAT s=SLEN { let (s,i) = s in TVec (CstLength (Z.to_int i, PHat (parse_builtin_prim s))) }
| s=prim_singl { TVec (CstLength (1, PHat s)) }
(* Containers (lists, args, tuples) *)
| LBRACE fs=separated_list(COMMA, ty_field) tl=optional_tail RBRACE
{ let pos,named,sym = split_fields fs in TList {pos;named;sym;tl} }
| ALPAREN fs=separated_list(COMMA, ty_non_sym_field) tl=optional_tail RPAREN
{ let pos',named',_ = split_fields fs in TArg' { tl'=tl ; pos' ; named' } }
| LPAREN pos_named=separated_list(COMMA, ty_named_field) tl=optional_tail named=optional_named RPAREN
{ TArg { tl ; pos=[] ; pos_named ; named } }
| T lst=separated_list(COMMA, simple_ty) RPAREN { TTuple lst }
(* C stuff *)
| STAR t=atomic_ty { TCPtr t }
| C i=cint RPAREN { TCConst i }
| C str=cstr RPAREN { TCConst str }
| PCI id=VARID RPAREN { TCConst (CIntVar id) }
| PCS id=VARID RPAREN { TCConst (CStrVar id) }

cint:
| i=INT { CIntSingl (Z.to_int i) }
| i1=INT? DPOINT i2=INT?
{ let i1,i2 = Option.map Z.to_int i1, Option.map Z.to_int i2 in CIntInterval (i1, i2) }

cstr:
| str=STRING { CStrSingl str }

%inline optional_tail:
| SEMICOLON ty=simple_ty { ty }
| ELLIPSIS { TOption TAny }
| { TOption TEmpty }

%inline optional_named:
| SEMICOLON named=separated_list(COMMA, ty_named_field) { named }
| { [] }

label:
| id=ID { id }
| s=SHORT { s }

%inline ty_field:
| lbl=SYMID COLON t=simple_ty { Sym (lbl, t) }
| t=ty_non_sym_field { t }

%inline ty_non_sym_field:
| lbl=label COLON t=simple_ty { Named (lbl, t) }
| t=simple_ty { Pos t }

%inline ty_named_field:
| lbl=label COLON t=simple_ty { (lbl, t) }

prim:
| LPAREN p=prim RPAREN { p }
| id=VARID { PVar (id) }
| id=ID { parse_builtin_prim id }
| s=SHORT { parse_builtin_prim s }
| s=prim_singl { s }
| p1=prim TOR p2=prim { PCup (p1, p2) }
| p1=prim TDIFF p2=prim { PDiff (p1, p2) }
| p1=prim TAND p2=prim { PCap (p1, p2) }
| TNEG p=prim { PNeg p }
| HAT p=prim { PHat p }
| PC id=VARID RPAREN { PChrVar id }
| LPAREN i1=INT? DPOINT i2=INT? RPAREN
{ let i1,i2 = Option.map Z.to_int i1, Option.map Z.to_int i2 in PInt' (i1,i2) }
| PI id=VARID RPAREN { PIntVar id }

prim_singl:
| TT { PLgl' true }
| FF { PLgl' false }
| str=STRING { PChr' str }
| i=INT { let i = Z.to_int i in PInt' (Some i, Some i) }
