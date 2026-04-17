open Sstt

type t = Pos of int | Named of string | Sym of sym
and sym = SLabel of t list | SStr of string

val name : t -> string
val sym_name : sym -> string
val of_name : string -> t
val sym_of_name : string -> sym
val get : t -> Label.t
val pos : int -> Label.t
val named : string -> Label.t
val sym : sym -> Label.t
val info : Label.t -> t
val equal : t -> t -> bool
val compare : t -> t -> int

module Reserved : sig
    val id : Label.t
    (** [id] defines a field used to identify arguments nominally. *)

    val npos : Label.t
    (** [npos] defines a field used to identify the number of
        positional parameters an argument defines/expects. *)

    val pos : Label.t
    (** [pos] defines a field used to identify the positional
        parameters an argument defines/expects. *)

    val named : Label.t
    (** [named] defines a field used to identify the named
        parameters an argument defines/expects. *)

    val card : Label.t
    (** [card] defines a field used to characterize the length of a vector. *)

    val content : Label.t
    (** [content] defines a field used to characterize the content of a Attr type. *)

    val classes : Label.t
    (** [classes] defines a field used to characterize the classes of a Attr type. *)

    val target : Label.t
    (** [target] defines a field used to characterize the target of a CPtr type. *)
end

val sym_of_ty : Ty.t -> sym list
type sym_selector = SelectLabel of t | SelectString of string
type sym_subst = { selector:sym_selector ; target:t }
val substitute : sym_subst list -> Ty.t -> Ty.t
