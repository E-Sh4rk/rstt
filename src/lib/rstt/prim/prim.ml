open Sstt

module Int = Int
module Chr = Chr
module Dbl = Dbl
module Raw = Raw
module Clx = Clx
module Lgl = Lgl
(* module Na = Na *)

let tag = Tag.mk "prim"
let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd
let mk p = Ty.cup p Na.any |> add_tag
let mk' p = add_tag p
let any_p = [Int.any ; Chr.any ; Dbl.any ; Raw.any ; Clx.any ; Lgl.any ; Na.any] |> Ty.disj
let any_p' = [Int.any ; Chr.any ; Dbl.any ; Raw.any ; Clx.any ; Lgl.any] |> Ty.disj
let na_p = Na.any
let any = any_p |> add_tag
let any' = any_p' |> add_tag
let destruct = proj_tag

type t = | TAny of bool (* na *)
         | TComp of Printer.descr * bool (* na *)
let to_t node ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
  then
    let na, pty = Ty.leq na_p pty, Ty.diff pty na_p in
    if Ty.leq any_p' pty then
        Some (TAny na)
    else
        Some (TComp (node ctx pty, na))
  else None

let hat_prec = 5
let hat_assoc = Prec.NoAssoc
let hat_sym = "^"
let hat_opinfo = (hat_sym, hat_prec, hat_assoc)
let map f = function TAny b -> TAny b | TComp (d,b) -> TComp (f d,b)
let print prec assoc fmt t =
  match t with
  | TAny true -> Format.fprintf fmt "any"
  | TAny false -> Prec.fprintf prec assoc hat_opinfo fmt "%sany" hat_sym
  | TComp (d,true) -> Format.fprintf fmt "%a" (Printer.print_descr_ctx prec assoc) d
  | TComp (d,false) ->
    Prec.fprintf prec assoc hat_opinfo fmt "%s%a" hat_sym (Printer.print_descr_ctx hat_prec hat_assoc) d

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
