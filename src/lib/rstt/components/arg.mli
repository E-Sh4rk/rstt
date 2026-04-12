open Sstt

val tag : Tag.t
type 'f atom = { pos_named : (string * 'f) list ; pos_tl: 'f ; named_tl : 'f ; named : (string * 'f) list }
type 'f atom' = { pos' : 'f list ; pos_tl': 'f ; named' : (string * 'f) list ; named_tl' : 'f }
type 'f elt =
| DefSite of 'f atom
| CallSite of  'f atom'
type 'f t = 'f elt list

val any : Ty.t
val mk : Ty.F.t atom -> Ty.t
val mk' : Ty.F.t atom' -> Ty.t
val destruct : Ty.t -> Ty.F.t t
val reidentify : id:Ty.t -> Ty.t -> Ty.t
val ids_of : Ty.t -> Enum.t list
val params_of_id : Enum.t -> unit atom
val map_atom : ('a -> 'b) -> 'a atom -> 'b atom
val map_atom' : ('a -> 'b) -> 'a atom' -> 'b atom'
val map : ('a -> 'b) -> 'a t -> 'b t
