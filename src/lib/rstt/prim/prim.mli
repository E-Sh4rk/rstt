open Sstt

val tag : Tag.t
val any : Ty.t
val any' : Ty.t
val mk : Ty.t -> Ty.t
val destruct : Ty.t -> Ty.t

module Int = Int
module Chr = Chr
module Dbl = Dbl
module Raw = Raw
module Clx = Clx
module Lgl = Lgl
