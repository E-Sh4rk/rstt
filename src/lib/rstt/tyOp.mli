open Sstt

val simplify : Ty.t -> Ty.t
val normalize_subst : Subst.t -> Subst.t option
val tally : MixVarSet.t -> Tallying.constr list -> Subst.t list
