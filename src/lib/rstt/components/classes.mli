open Sstt

type 'r tail =
| NoOther | AllOthers | Unknown
| RowVars of ('r list * 'r list) list
type 'r atom = attrs * attrs * 'r tail
and attrs = line list
and line = L of string * attrs

val define_class : string -> subclass:(string list) -> unit
val is_defined : string -> bool
val mk : RowVar.t atom -> Ty.t
val any : Ty.t
val noclass : Ty.t
val map_atom : (('a -> 'b) -> 'a atom -> 'b atom)
val destruct : Ty.t -> RowVar.t atom
