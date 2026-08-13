open Sstt

val tag : Tag.t
type ('l,'f) atom = { pos_named : ('l * 'f) list ; pos_tl: 'f ; named_tl : 'f ; named : ('l * 'f) list }
type ('l,'f) atom' = { pos' : 'f list ; pos_tl': 'f ; named' : ('l * 'f) list ; named_tl' : 'f }
type ('l,'f) elt =
| DefSite of ('l,'f) atom
| CallSite of  ('l,'f) atom'
type ('l,'f) t = ('l,'f) elt list

val any : Ty.t
val mk : (string, Ty.F.t) atom -> Ty.t
val mk' : (string, Ty.F.t) atom' -> Ty.t
val destruct : Ty.t -> (string, Ty.F.t) t
val reidentify : id:Ty.t -> Ty.t -> Ty.t
val ids_of : Ty.t -> Enum.t list
val params_of_id : Enum.t -> (string, unit) atom
val map_atom : ('l -> 'm) -> ('a -> 'b) -> ('l,'a) atom -> ('m,'b) atom
val map_atom' : ('l -> 'm) -> ('a -> 'b) -> ('l,'a) atom' -> ('m,'b) atom'
val map : ('a -> 'b) -> ('l,'a) t -> ('l,'b) t
