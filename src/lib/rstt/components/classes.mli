open Sstt

(* A tail [Tail (kind, dnf)] denotes the union of the row variable formula [dnf]
   (a DNF of positive and negative row variables) with: all the other classes
   for [PresentOr], none of them for [AbsentOr], and nothing for [Exactly].
   [Unknown] leaves the other classes unspecified. *)
type tail_kind = PresentOr | AbsentOr | Exactly
type 'r tail =
| Unknown
| Tail of tail_kind * ('r list * 'r list) list
type 'r atom = { pos:attrs ; neg:attrs ; unk:string list ; tail:'r tail }
and attrs = line list
and line = L of string * attrs

val add_relation : string -> supers:(string list) -> unit
val build_lattice : unit -> unit
val class_of_method : string -> string
val mk : RowVar.t atom -> Ty.t
val any : Ty.t
val noclass : Ty.t
val map_atom : (('a -> 'b) -> 'a atom -> 'b atom)
val destruct : Ty.t -> RowVar.t atom
