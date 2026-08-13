open Sstt

type label =
| LConst of string (** A regular (constant) label. *)
| LVar of string   (** A symbolic label variable. *)

type ('v,'r,'i) ty =
| FLVar of string  (** A symbolic label variable that can be matched with a singleton string argument. *)
| FRegular of ('v,'r,'i) Builder.t
| FList of ('v,'r,'i) lst
| FAttr of (('v,'r,'i) ty, 'r classes) Attr.atom

and ('v,'r,'i) lst = {
    bindings: (label * ('v,'r,'i) ty) list ;
    tl: ('v,'r,'i) ty
}

and ('v,'r,'i) arg = {
    pos_named : (label * ('v,'r,'i) ty) list ;
    pos_tl: ('v,'r,'i) ty ;
    named_tl : ('v,'r,'i) ty ;
    named : (label * ('v,'r,'i) ty) list
}

type ('v,'r,'i) t = { dom: ('v,'r,'i) arg ; ret: ('v,'r,'i) ty }

val resolve : Builder.env -> (string,string,string) t -> Builder.env * (Var.t,RowVar.t,TId.t) t

val is_regular_ty : ('v,'r,'i) t -> bool
(** Returns true if and only if its argument does not contain any FunSig specific construct
    (e.g. polymorphic labels LVar and FLVar).
*)

val to_regular : ('v,'r,'i) t -> ('v,'r,'i) Builder.t
(** Converts its argument [t] to a regular builder type.
    @raise [Invalid_argument] if the [is_regular_ty t] is false.
*)

val specialize : (Var.t,RowVar.t,TId.t) t -> Ty.t -> (Var.t,RowVar.t,TId.t) Builder.t
(** [specialize t arg] converts [t] into a regular builder type,
    in the context where the function it represents is given an argument [arg]
    that will be used to resolve FunSig specific constructs.
    @raise [Invalid_argument] if some FunSig specific constructs remain after specialization.
*)

(* TODO:
1. Implement this interface
2. Implement a new entry funsig_main in the parser that parses a fun sig,
   always prioritizing FunSig constructors (e.g. FList ...) over regular ones (e.g. FRegular (TList ...))
   when possible (but if the expression uses an operator not supported by FunSig, e.g. a set connective,
   then backup to a FRegular type; or fail if not possible, e.g. if the expression to parse is not a single arrow)
3. Remove old symbolic labels system (in module Labels and Lst) 
*)
