open Sstt

(* A tail [Tail (kind, dnf)] denotes the union of the row variable formula [dnf]
   (a DNF of positive and negative row variables) with: all the other classes
   for [PresentOr], none of them for [AbsentOr], and nothing for [Exactly].
   [Unknown] leaves the other classes unspecified. *)
type tail_kind = PresentOr | AbsentOr | Exactly
type 'r tail =
| Unknown
| Tail of tail_kind * ('r list * 'r list) list
(* Method classes are written [@m], and classes [c]. Names that are not
   identifiers are quoted: [@"m"] and ["c"]. *)
type cls = Class of string | Method of string
type 'r atom = { pos:attrs ; neg:cls list ; unk:attrs ; tail:'r tail }
and attrs = line list
and line = L of cls * attrs

val string_of_cls : cls -> string
val cls_of_string : string -> cls
val pp_cls : Format.formatter -> cls -> unit

val declare_class : string -> methods:(string list) -> unit
val mk : RowVar.t atom -> Ty.t
val any : Ty.t
val noclass : Ty.t
val map_atom : (('a -> 'b) -> 'a atom -> 'b atom)
val destruct : Ty.t -> RowVar.t atom
