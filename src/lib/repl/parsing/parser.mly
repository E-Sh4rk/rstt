%{
open Ast
open Rstt.Builder
open Rstt

let parse_id_or_builtin str =
    match str with
    | "empty" -> TEmpty
    | "any" -> TAny
    | "dyn" -> TDyn
    | "attr" -> TAttrAny
    | "absent" -> TOption TEmpty
    | "null" -> TNull
    | "env" -> TEnv
    | "sym" -> TSym
    | "lang" -> TLang
    | "prim" -> TPrim PAny
    | "list" -> TList{bindings=[];sym=[];tl=TOption TAny}
    (* C stuff  *)
    | "c_double" -> TCConst CDouble
    | "c_string" -> TCConst CString
    | "c_char" -> TCConst CChar
    | "c_void" -> TCConst CVoid
    | "c_null" -> TCConst CNull
    | "c_int_na" -> TCConst CIntNa
    | "c_int" -> TCConst CInt
    | "c_na" -> TCConst CNa
    | "c_bool" -> TCConst CBool
    | "c_true" -> TCConst CTrue
    | "c_false" -> TCConst CFalse
    | "c_ptr" -> TCConst CPtr
    | str -> TId str

let parse_builtin_prim str =
    match str with
    | "vec" -> PAny
    | "lgl" -> PSubLgl
    | "chr" -> PSubChr
    | "int" -> PSubInt
    | "dbl" -> PSubDbl
    | "clx" -> PSubClx
    | "raw" -> PSubRaw
    | "LGL" -> PLgl
    | "CHR" -> PChr
    | "INT" -> PInt
    | "DBL" -> PDbl
    | "CLX" -> PClx
    | "RAW" -> PRaw
    | "NUM" -> PNum
    | str -> raise (Errors.E_Parser ("Unknown primitive builtin "^str))

let assert_one i =
    if Z.equal i Z.one |> not
    then raise (Errors.E_Parser ("Cannot specify a size other than 1 for a vector"))

type arg_elt = Pos of ty | Named of string * ty | Tail of ty * ty
let split_arg_elt' lst =
    let rec pos_fields lst =
        match lst with
        | (Pos t)::lst ->
            let ts, lst = pos_fields lst in
            (t::ts), lst
        | _ -> [], lst
    in
    let rec named_fields lst =
        match lst with
        | (Named (str,ty))::lst ->
            let fs, lst = named_fields lst in
            ((str,ty)::fs), lst
        | _ -> [], lst
    in
    let tail_field lst =
        match lst with
        | [] -> TOption TEmpty, TOption TEmpty
        | [Tail (t1,t2)] -> t1, t2
        | _ -> raise (Errors.E_Parser ("Unexpected field"))
    in
    let pos, lst = pos_fields lst in
    let named, lst = named_fields lst in
    pos, named, tail_field lst
let split_arg_elt lst =
    let rec named_fields lst =
        match lst with
        | (Named (str,ty))::lst ->
            let fs, lst = named_fields lst in
            ((str,ty)::fs), lst
        | _ -> [], lst
    in
    let tail_field lst =
        match lst with
        | (Tail (t1,t2))::lst -> (t1, t2), lst
        | lst -> (TOption TEmpty, TOption TEmpty), lst
    in
    let pos_named, lst = named_fields lst in
    let tl, lst = tail_field lst in
    let named, lst = named_fields lst in
    if List.is_empty lst
    then pos_named, tl, named
    else raise (Errors.E_Parser ("Unexpected field"))

let split_lst_elts lst =
    let lst, tl = match List.rev lst with
    | (`LstTl ty)::lst -> List.rev lst, ty
    | lst -> List.rev lst, TOption TEmpty
    in
    let bindings, sym = lst |> List.partition_map (function
        | `LstNamed (str,t) -> Either.left (str,t)
        | `LstSym (str,t) -> Either.right (Labels.sym_of_name str,t)
        | `LstTl _ -> raise (Errors.E_Parser ("Unexpected list tail"))
    ) in
    bindings, sym, tl

let split_classes_elts lst =
    let lst, tl = match List.rev lst with
    | `ClassAllOthers::lst -> List.rev lst, Classes.AllOthers
    | `ClassEllipsis::lst -> List.rev lst, Classes.Unknown
    | (`ClassRowVar id)::lst -> List.rev lst, Classes.RowVars ([[id],[]])
    | lst -> List.rev lst, Classes.NoOther
    in
    let pos, lst = lst |> List.partition_map (function
        | `ClassId id -> Either.left (Classes.L (id, []))
        | `ClassNoId id -> Either.right (`ClassNoId id)
        | `ClassMaybeId id -> Either.right (`ClassMaybeId id)
        | _ -> raise (Errors.E_Parser ("Unexpected class tail"))
    ) in
    let neg, unk = lst |> List.partition_map (function
        | `ClassNoId id -> Either.left (Classes.L (id, []))
        | `ClassMaybeId id -> Either.right (Classes.L (id, []))
    ) in
    pos, neg, unk, tl
%}

%token<string> STRING, SHORT(*, SBRACKET*)
%token<Z.t> INT, LINT, DINT, VLEN
%token<string> ID, VARID, RVARID, SYMID
%token<string*Z.t> SLEN
%token TYPE WHERE AND
%token BREAK COMMA EQUAL COLON SEMICOLON ELLIPSIS
%token C VP (*VB*) P S HAT ARROW CARROW STAR WITH
%token PI PD PC PN PCI PCINA PCS
%token TT FF EPTR_ANY EPTR
%token QUESTION_MARK DPOINT
%token LPAREN RPAREN LBRACE RBRACE ALPAREN
%token LBRACKET RBRACKET LLBRACKET RRBRACKET
%token LEQ GEQ LT GT
%token TOR TAND TNEG TDIFF
%token EOF

%start<program> program
%start<ty> ty_main
%start<command> command

%right ARROW CARROW
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
| o=printing_options e=expr BREAK { Expr (o, e) }

%inline printing_options:
|       { { name=None ; raw=false } }
| COLON { { name=None ; raw=false } }
| COLON COLON { { name=None ; raw=true } }
| str=STRING COLON { { name=Some str ; raw=false } }
| str=STRING COLON COLON { { name=Some str ; raw=true } }

expr:
| e=expr_nocmp { e }
| e1=expr_nocmp op=op e2=expr_nocmp { CCmp (e1, op, e2) }

expr_nocmp:
| e=simpl_expr { e }
| e1=expr_nocmp SEMICOLON e2=simpl_expr { CCat (e1, e2) }
| e1=expr_nocmp e2=atomic_expr { CApp (e1, e2) }

simpl_expr:
| s=tsubst { CSubst s }
| t=tally { CTally t }
| ty=ty { CTy ty }
| LLBRACKET e=expr_nocmp RRBRACKET { e }

atomic_expr:
| s=tsubst { CSubst s }
| t=tally { CTally t }
| ty=atomic_ty { CTy ty }
| LLBRACKET e=expr_nocmp RRBRACKET { e }

op:
| LEQ { LEQ } | EQUAL { EQ } | GEQ { GEQ }

tsubst:
| LLBRACKET bindings=separated_list(SEMICOLON, subst_binding) RRBRACKET { bindings }

%inline subst_binding:
| v=VARID COLON ty=ty { (v, ty) }

tally:
| LLBRACKET cs=separated_nonempty_list(SEMICOLON, tally_binding) RRBRACKET { cs }

%inline tally_binding:
| ty1=ty op=op ty2=ty { (ty1, op, ty2) }

ty_main:
| ty=ty EOF { ty }

classes:
| LT elts=separated_list(COMMA, classes_elt) GT
{
    let pos, neg, unk, tail = split_classes_elts elts in
    { pos ; neg ; unk ; tail }
}

%inline classes_elt:
| STAR { `ClassAllOthers } | ELLIPSIS { `ClassEllipsis }
| id=RVARID { `ClassRowVar id }
| id=ID { `ClassId id } | TNEG id=ID { `ClassNoId id }
| QUESTION_MARK id=ID { `ClassMaybeId id }

ty:
| ty=simple_ty { ty }
| ty=simple_ty WHERE ts=separated_nonempty_list(AND, param_type_def)
  { TWhere (ty, ts) }

%inline param_type_def:
| name=ID EQUAL t=simple_ty { (name, t) }

simple_ty:
| ty=atomic_ty classes=classes { TAttr {content=ty;classes=CClasses classes;attrs=TAny} }
| ty=atomic_ty classes=classes WITH attrs=atomic_ty { TAttr {content=ty;classes=CClasses classes;attrs=attrs} }
| classes=classes { TAttr {content=TAny;classes=CClasses classes;attrs=TAny} }
| classes=classes WITH attrs=atomic_ty { TAttr {content=TAny;classes=CClasses classes;attrs=attrs} }
| ty=atomic_ty { ty }
| ty=atomic_ty WITH attrs=atomic_ty { TAttr {content=ty;classes=CAny;attrs=attrs} }
| ty1=simple_ty TOR ty2=simple_ty { TCup (ty1, ty2) }
| ty1=simple_ty TDIFF ty2=simple_ty { TDiff (ty1, ty2) }
| ty1=simple_ty TAND ty2=simple_ty { TCap (ty1, ty2) }
| TNEG ty=simple_ty { TNeg (ty) }
| ty1=simple_ty ARROW ty2=simple_ty { TArrow (ty1, ty2) }
| ty1=simple_ty CARROW ty2=simple_ty { TCArrow (ty1, ty2) }
| ty=atomic_ty QUESTION_MARK { TOption (ty) }

atomic_ty:
| id=ID { parse_id_or_builtin id }
| id=VARID { TVar (id) }
| id=RVARID { TRowVar (id) }
| LPAREN ty=ty RPAREN { ty }
| P p=prim RPAREN { TPrim p }
| S s=ty RPAREN { TStruct s }
(* Vectors *)
| VP p=prim RPAREN { TVec (Vector p) }
| s=SHORT { TVec (Vector (parse_builtin_prim s)) }
| HAT s=SHORT { TVec (Vector (PHat (parse_builtin_prim s))) }
| s=SLEN { let (s,i) = s in assert_one i ; TVec (Scalar (parse_builtin_prim s)) }
| HAT s=SLEN { let (s,i) = s in assert_one i ; TVec (Scalar (PHat (parse_builtin_prim s))) }
// | VB l=prim RBRACKET LPAREN p=prim RPAREN { TVec (VarLength (l,p)) }
// | s=SBRACKET l=prim RBRACKET {TVec (VarLength (l,parse_builtin_prim s)) }
// | HAT s=SBRACKET l=prim RBRACKET { TVec (VarLength (l,PHat (parse_builtin_prim s))) }
| i=VLEN LPAREN p=prim RPAREN { assert_one i ; TVec (Scalar (p)) }
| s=prim_atom { TVec (Scalar s) }
(* Containers (lists, args, tuples, externalptr) *)
| EPTR_ANY { TExtPtr } | EPTR ty=ty RPAREN { TExtPtr' ty }
| LBRACE elts=separated_list(COMMA, lst_elt) RBRACE
{ let bindings,sym,tl = split_lst_elts elts in TList {bindings;sym;tl} }
| ALPAREN elts=separated_list(COMMA, arg_elt2) RPAREN
{
    let pos',named',tl' = split_arg_elt' elts in
    let pos_tl',named_tl' = tl' in
    TArg' { pos' ; named' ; pos_tl' ; named_tl' }
}
| LPAREN elts=separated_list(COMMA, arg_elt) RPAREN
{
    let pos_named,tl,named = split_arg_elt elts in
    let pos_tl,named_tl = tl in
    TArg { pos_named ; pos_tl ; named ; named_tl }
}
| LBRACKET lst=separated_list(COMMA, simple_ty) RBRACKET { TTuple lst }
(* C stuff *)
| STAR t=atomic_ty { TCPtr t }
| C i=cint RPAREN { TCConst i }
| C str=cstr RPAREN { TCConst str }
| PCI id=VARID RPAREN { TCConst (CIntVar id) }
| PCINA id=VARID RPAREN { TCConst (CIntNaVar id) }
| PCS id=VARID RPAREN { TCConst (CStrVar id) }

cint:
| i=INT { CIntSingl (Z.to_int i) }
| i1=INT? DPOINT i2=INT?
{ let i1,i2 = Option.map Z.to_int i1, Option.map Z.to_int i2 in CIntInterval (i1, i2) }

cstr:
| str=STRING { CStrSingl str }

%inline lst_elt:
| lbl=SYMID COLON t=simple_ty { `LstSym (lbl, t) }
| lbl=label COLON t=simple_ty { `LstNamed (lbl, t) }
| ty=simple_ty { `LstTl ty }

label:
| id=ID { id }
| s=SHORT { s }

arg_elt2:
| lbl=label COLON t=simple_ty { Named (lbl, t) }
| t=simple_ty { Pos t }
| ELLIPSIS COLON ty=simple_ty { Tail (ty, ty) }
| ELLIPSIS COLON LPAREN ty1=simple_ty COMMA ty2=simple_ty RPAREN { Tail (ty1, ty2) }

arg_elt:
| lbl=label COLON t=simple_ty { Named (lbl, t) }
| lbl=label EQUAL id=SYMID { Named (lbl, TSymLabel (id)) }
| ELLIPSIS COLON ty=simple_ty { Tail (ty, ty) }
| ELLIPSIS COLON LPAREN ty1=simple_ty COMMA ty2=simple_ty RPAREN { Tail (ty1, ty2) }

prim:
| LPAREN p=prim RPAREN { p }
| id=VARID { PVar (id) }
| s=SHORT { parse_builtin_prim s }
| s=prim_atom { s }
| p1=prim TOR p2=prim { PCup (p1, p2) }
| p1=prim TDIFF p2=prim { PDiff (p1, p2) }
| p1=prim TAND p2=prim { PCap (p1, p2) }
| TNEG p=prim { PNeg p }
| HAT p=prim { PHat p }

prim_atom:
(* Lgl *)
| TT { PLgl' true }
| FF { PLgl' false }
(* Chr *)
| str=STRING { PChr' str }
(* Dbl *)
| i=DINT { let i = Z.to_int i in PDbl' (Some i, Some i) }
| LPAREN i1=dint_opt DPOINT i2=dint_opt RPAREN
{ let i1,i2 = Option.map Z.to_int i1, Option.map Z.to_int i2 in PDbl' (i1,i2) }
(* Int *)
| i=LINT { let i = Z.to_int i in PInt' (Some i, Some i) }
| LPAREN i1=LINT DPOINT i2=LINT? RPAREN
{ let i1,i2 = Some (Z.to_int i1), Option.map Z.to_int i2 in PInt' (i1,i2) }
| LPAREN DPOINT i2=LINT RPAREN
{ let i1,i2 = None, Some (Z.to_int i2) in PInt' (i1,i2) }
(* Num *)
| i=INT { let i = Z.to_int i in PNum' (Some i, Some i) }
| LPAREN i1=INT DPOINT i2=INT? RPAREN
{ let i1,i2 = Some (Z.to_int i1), Option.map Z.to_int i2 in PNum' (i1,i2) }
| LPAREN DPOINT i2=INT RPAREN
{ let i1,i2 = None, Some (Z.to_int i2) in PNum' (i1,i2) }
(* Vars *)
| PC id=VARID RPAREN { PChrVar id }
| PI id=VARID RPAREN { PIntVar id }
| PD id=VARID RPAREN { PDblVar id }
| PN id=VARID RPAREN { PNumVar id }

%inline dint_opt: { None } | i=DINT { Some i }
