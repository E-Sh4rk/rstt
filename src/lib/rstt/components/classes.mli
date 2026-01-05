open Sstt

type 'r tail =
| Closed | Open
| RowVars of ('r list * 'r list) list
type 'r atom = attrs * attrs * 'r tail
and attrs = line list
and line = L of string * attrs

val define_class : name:string -> subclass:(string list) -> unit
val mk : RowVar.t atom -> Ty.t
val any : Ty.t
val map_atom : (('a -> 'b) -> 'a atom -> 'b atom)
val destruct : Ty.t -> RowVar.t atom
