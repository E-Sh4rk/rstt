open Sstt

let na = Enum.mk "na"
let na_ty = Descr.mk_enum na |> Ty.mk_descr

module Hat = struct
  let sym () = format_of_string "^"
  let prec = 5
  let assoc = Prec.NoAssoc
  let opinfo () = (sym (), prec, assoc)
end

module type PrimComp = sig
    open Printer
    val tag_name : string
    val any : Ty.t
    type t
    val any_t : t
    val to_t : build_ctx -> Ty.t -> t option
    val map : (descr -> descr) -> t -> t
    val print : (int -> Prec.assoc -> Format.formatter -> t -> unit)
    val is_singleton : Ty.t -> bool
end
module MakeCompWithNa(P:PrimComp) = struct
  type 'a t = WithNa of 'a | WithoutNa of 'a | Na
  let tag = Tag.mk P.tag_name
  let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
  let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd

  let any_p = Ty.cup P.any na_ty
  let any_p' = P.any
  let any = add_tag any_p
  let any' = add_tag any_p'
  let mk ty = Ty.cup ty na_ty |> add_tag
  let mk' ty = ty |> add_tag

  let destruct ty =
    let ty = proj_tag ty in
    let ty, na = Ty.diff ty na_ty, Ty.leq na_ty ty in
    if Ty.is_empty ty then Na
    else if na then WithNa ty
    else WithoutNa ty

  let is_singleton ty =
    match destruct ty with
    | WithNa _ | Na -> false
    | WithoutNa ty -> P.is_singleton ty

  let map f = function Na -> Na | WithNa t -> WithNa (P.map f t) | WithoutNa t -> WithoutNa (P.map f t)
  let to_t ctx comp =
    let (_, pty) = Op.TagComp.as_atom comp in
    if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty) then
      let pty, na = Ty.diff pty na_ty, Ty.leq na_ty pty in
      if Ty.is_empty pty then Some Na
      else if na then P.to_t ctx pty |> Option.map (fun t -> WithNa t)
      else P.to_t ctx pty |> Option.map (fun t -> WithoutNa t)
    else
      None

  let print prec assoc fmt t =
    let print_without_na prec assoc fmt t =
      Prec.print_unary P.print prec assoc (Hat.opinfo ()) fmt t
    in
    match t with
    | WithNa t -> P.print prec assoc fmt t
    | WithoutNa t -> print_without_na prec assoc fmt t
    | Na ->
      Prec.print_binary_op' P.print print_without_na
        prec assoc Diff fmt P.any_t P.any_t
end