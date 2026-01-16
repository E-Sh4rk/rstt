open Sstt

let double = Enum.mk "c_double" |> Descr.mk_enum |> Ty.mk_descr
let str = Enum.mk "c_string" |> Descr.mk_enum |> Ty.mk_descr
let char = Enum.mk "c_char" |> Descr.mk_enum |> Ty.mk_descr
let void = Enum.mk "c_void" |> Descr.mk_enum |> Ty.mk_descr
