open Sstt
open Printer

let na = Enum.mk "na"
let na_ty = Descr.mk_enum na |> Ty.mk_descr

module Hat = struct
  let sym = "^"
  let prec = 5
  let assoc = Prec.NoAssoc
  let opinfo = (sym, prec, assoc)
end

module type PrimComp = sig
    val tag_name : string
    val any : Ty.t
    type t
    val any_t : t
    val to_t : (ctx -> Ty.t -> descr) -> ctx -> Ty.t -> t option
    val map : (descr -> descr) -> t -> t
    val print : (int -> Prec.assoc -> Format.formatter -> t -> unit)
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

  let map f = function Na -> Na | WithNa t -> WithNa (P.map f t) | WithoutNa t -> WithoutNa (P.map f t)
  let to_t node ctx comp =
    let (_, pty) = Op.TagComp.as_atom comp in
    if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty) then
      let pty, na = Ty.diff pty na_ty, Ty.leq na_ty pty in
      if Ty.is_empty pty then Some Na
      else if na then P.to_t node ctx pty |> Option.map (fun t -> WithNa t)
      else P.to_t node ctx pty |> Option.map (fun t -> WithoutNa t)
    else
      None

  let print prec assoc fmt t =
    let print_without_na prec assoc fmt t =
      Prec.fprintf prec assoc Hat.opinfo fmt "%s%a" Hat.sym
        (P.print Hat.prec NoAssoc) t
    in
    match t with
    | WithNa t -> P.print prec assoc fmt t
    | WithoutNa t -> print_without_na prec assoc fmt t
    | Na ->
      let sym,prec',_ as opinfo = Prec.binop_info Diff in
      Prec.fprintf prec assoc opinfo fmt "%a%s%a"
        (P.print prec' Left) P.any_t
        sym
        (print_without_na prec' Right) P.any_t

end