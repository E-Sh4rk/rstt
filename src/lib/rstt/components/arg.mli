open Sstt

val tag : Tag.t
type ('f, 't) atom = { pos_named : (string * 'f) list ; pos_tl: 't ; named_tl : 'f ; named : (string * 'f) list }
type ('f, 't) atom' = { pos' : 'f list ; pos_tl': 't ; named' : (string * 'f) list ; named_tl' : 'f }
type ('f, 't) elt =
| DefSite of ('f, 't) atom
| CallSite of  ('f, 't) atom'
type ('f, 't) t = ('f, 't) elt list

val any : Ty.t
val mk : (Ty.F.t, Ty.t) atom -> Ty.t
val mk' : (Ty.F.t, Ty.t) atom' -> Ty.t
val destruct : Ty.t -> (Ty.F.t, Ty.t) t
val reidentify : id:Ty.t -> Ty.t -> Ty.t
val ids_of : Ty.t -> Enum.t list
val params_of_id : Enum.t -> (unit, unit) atom
val map_atom : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) atom -> ('b,'d) atom
val map_atom' : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) atom' -> ('b,'d) atom'
val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t
