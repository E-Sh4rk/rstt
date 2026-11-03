val safe_mode : bool ref

(** [set name value] changes the setting [name] from its textual [value].
    @raise Invalid_argument if the setting is unknown or the value invalid. *)
val set : string -> string -> unit
