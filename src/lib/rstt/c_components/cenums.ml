open Sstt

let double = Enum.mk "C_double" |> Descr.mk_enum |> Ty.mk_descr
let str = Enum.mk "C_str" |> Descr.mk_enum |> Ty.mk_descr
let char = Enum.mk "C_char" |> Descr.mk_enum |> Ty.mk_descr
let void = Enum.mk "C_void" |> Descr.mk_enum |> Ty.mk_descr
