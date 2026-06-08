open Sstt

val tag : Tag.t
val any : Ty.t
val any' : Ty.t
val mk : Ty.t -> Ty.t
val destruct : Ty.t -> Ty.t

val partition : Ty.t list (* TODO: remove *)
val is_simple : Ty.t -> bool
val is_singleton : Ty.t -> bool

(* TODO *)
(*
val is_whole : Ty.t -> bool
val enlarge : Ty.t -> Ty.t (* Enlarge the type so that it uses only 'whole' primitive types *)
*)

module Int = Int
module Chr = Chr
module Dbl = Dbl
module Raw = Raw
module Clx = Clx
module Lgl = Lgl
