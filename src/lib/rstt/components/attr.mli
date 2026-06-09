open Sstt

type ('a, 'c) atom = { content:'a ; classes:'c ; attrs:'a (* should be a Lst type *) }
type ('a, 'c) line = ('a, 'c) atom list * ('a, 'c) atom list
type ('a, 'c) t = ('a, 'c) line list

val tag : Tag.t
val mk : (Ty.t, Ty.t) atom -> Ty.t
val mk_content : Ty.t -> Ty.t
val mk_content_noattr : Ty.t -> Ty.t
val mk_line : (Ty.t,Ty.t) line -> Ty.t
val any : Ty.t

val destruct : Ty.t -> (Ty.t, Ty.t) t
val proj_content : Ty.t -> Ty.t
val proj_classes : Ty.t -> Ty.t
val proj_attrs : Ty.t -> Ty.t

val map_atom : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) atom -> ('b, 'd) atom
val map_line : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) line -> ('b, 'd) line
val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
