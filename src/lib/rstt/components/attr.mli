open Sstt

type ('a, 'c) atom = { content:'a ; classes:'c option }
type ('a, 'c) line = ('a, 'c) atom list * ('a, 'c) atom list
type ('a, 'c) t = ('a, 'c) line list

val mk : (Ty.t, Ty.t) atom -> Ty.t
val mk_noclass : Ty.t -> Ty.t
val mk_anyclass : Ty.t -> Ty.t
val any : Ty.t
val destruct : Ty.t -> (Ty.t, Ty.t) t

val map_atom : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) atom -> ('b, 'd) atom
val map_line : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) line -> ('b, 'd) line
val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
