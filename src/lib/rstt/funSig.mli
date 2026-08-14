open Sstt

type label =
| LConst of string (** A regular (constant) label. *)
| LVar of string   (** A symbolic label variable. *)

type ('v,'r,'i) ty =
| FLVar of string  (** A symbolic label variable that can be matched with a singleton string argument. *)
| FRegular of ('v,'r,'i) Builder.t
| FList of (label, ('v,'r,'i) ty) Lst.atom
| FAttr of (('v,'r,'i) ty, 'r Builder.classes) Attr.atom

type ('v,'r,'i) arg = (label, ('v,'r,'i) ty) Arg.atom

type ('v,'r,'i) t = { dom: ('v,'r,'i) arg ; ret: ('v,'r,'i) ty }

val resolve : Builder.env -> (string,string,string) t -> Builder.env * (Var.t,RowVar.t,Builder.TId.t) t

val is_regular_ty : ('v,'r,'i) t -> bool
(** Returns true if and only if its argument does not contain any FunSig specific construct
    (e.g. polymorphic labels LVar and FLVar).
*)

val to_regular : ?polymorphic:bool -> ('v,'r,'i) t -> ('v,'r,'i) Builder.t
(** Converts its argument [t] to a regular builder type.
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
  (Var.t,RowVar.t,Builder.TId.t) t list
(** [specialize t arg] instantiates the polymorphic labels of [t],
    in the context where the function it represents is given an argument [arg]
    that will be used to resolve them. It returns one signature per possible
    instantiation (their conjunction should be considered).
    Polymorphic labels that cannot be resolved from [arg] are left as is.
    @raise [Invalid_argument] if a polymorphic label cannot be instantiated
    (for instance, because of conflicting constraints).
*)
