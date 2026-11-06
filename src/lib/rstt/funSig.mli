open Sstt

type gvar = { group: string ; column: string }
(** A column [column] of the group [group], i.e. a family of labels indexed by
    the instances of [group]. *)

type label =
| LConst of string (** A regular (constant) label. *)
| LVar of string   (** A symbolic label variable. *)
| LGroup of gvar   (** A repetition: the binding it labels is replicated once per instance of the group. *)

type ('v,'r,'i) ty =
| FLVar of string  (** A symbolic label variable that can be matched with a singleton string argument. *)
| FGVar of gvar    (** A group column in type position, denoting the singleton string of its label. *)
| FRegular of ('v,'r,'i) Builder.t
| FList of (label, ('v,'r,'i) ty) Lst.atom
| FAttr of (('v,'r,'i) ty, 'r Builder.classes) Attr.atom

type ('v,'r,'i) arg = (label, ('v,'r,'i) ty) Arg.atom

type ('v,'r,'i) arrow = { dom: ('v,'r,'i) arg ; ret: ('v,'r,'i) ty }

type ('v,'r,'i) t = ('v,'r,'i) arrow list
(** A signature: the intersection of its arrows (there must be at least one).
    The arrows share their type variables, but their label variables and
    groups are local to each of them. *)

val repetition : string -> string -> ('v,'r,'i) ty -> label * ('v,'r,'i) ty
(** [repetition g key body] builds the repetition [(#key: body)_g]: the
    variables of [body] whose name ends with ["_g"] are the columns of [g], and
    are rewritten accordingly.
    @raise [Invalid_argument] if [key] is not a column of [g].
*)

val resolve : Builder.env -> (string,string,string) t -> Builder.env * (Var.t,RowVar.t,Builder.TId.t) t
(** Resolves the identifiers of the arrows of a signature, in turn: a type
    variable that occurs in several arrows is the same in all of them. *)

val is_regular_ty : ('v,'r,'i) t -> bool
(** Returns true if and only if its argument does not contain any FunSig specific construct
    (e.g. polymorphic labels LVar and FLVar).
*)

val to_regular : ?polymorphic:bool -> ('v,'r,'i) t -> ('v,'r,'i) Builder.t
(** Converts its argument [t] to a regular builder type (the intersection of
    its arrows).
    If [polymorphic] is true (it defaults to false), the domain of the
    resulting arrow is built with [TPolyArg] instead of [TArg].
    @raise [Invalid_argument] if the [is_regular_ty t] is false.
*)

val to_regular_ty : ('v,'r,'i) ty -> ('v,'r,'i) Builder.t
(** Converts a FunSig type to a regular builder type.
    @raise [Invalid_argument] if it contains any polymorphic label.
*)

val to_regular_arg : ?polymorphic:bool -> ('v,'r,'i) arg -> ('v,'r,'i) Builder.t
(** Converts a FunSig argument to a regular builder type ([TArg], or
    [TPolyArg] if [polymorphic] is true; it defaults to false).
    @raise [Invalid_argument] if it contains any polymorphic label.
*)

val specialize : (Var.t,RowVar.t,Builder.TId.t) t -> Ty.t ->
  (Var.t,RowVar.t,Builder.TId.t) t
(** [specialize t arg] instantiates the label variables and the groups of [t],
    in the context where the function it represents is given an argument [arg].
    Each arrow of [t] is specialized independently.
    It never fails: a variable or a group that the argument does not determine
    is left as is, and [to_regular] is the operation that reports it.
    See [fun_schemes.md] for the algorithm.
*)
