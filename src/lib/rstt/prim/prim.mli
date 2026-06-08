open Sstt

val tag : Tag.t
val any : Ty.t
val any' : Ty.t
val mk : Ty.t -> Ty.t
val destruct : Ty.t -> Ty.t

val is_simple : Ty.t -> bool
val is_singleton : Ty.t -> bool
val is_whole : Ty.t -> bool
(* return true if an only if the primitive type is only composed of [any] components *)

module Int = Int
module Chr = Chr
module Dbl = Dbl
module Raw = Raw
module Clx = Clx
module Lgl = Lgl
