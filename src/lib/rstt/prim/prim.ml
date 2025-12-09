open Sstt

module Int = Int
module Chr = Chr
module Dbl = Dbl
module Raw = Raw
module Clx = Clx
module Lgl = Lgl

let tag = Tag.mk "prim"
let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd
let mk p = add_tag p
let any_p = [Int.any ; Chr.any ; Dbl.any ; Raw.any ; Clx.any ; Lgl.any] |> Ty.disj
let any_p' = [Int.any' ; Chr.any' ; Dbl.any' ; Raw.any' ; Clx.any' ; Lgl.any'] |> Ty.disj
let any = any_p |> add_tag
let any' = any_p' |> add_tag
let destruct = proj_tag

type t = | TAny | TAny' | TComp of Printer.descr
let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
  then
    if Ty.leq any_p pty then Some TAny
    else if Ty.equiv any_p' pty then Some TAny'
    else Some (TComp (ctx.Printer.build pty))
  else None
let map f = function TAny -> TAny | TAny' -> TAny' | TComp d -> TComp (f d)
let print prec assoc fmt t =
  match t with
  | TAny -> Format.fprintf fmt "any"
  | TAny' -> Prec.fprintf prec assoc Na.Hat.opinfo fmt "^any"
  | TComp d -> Format.fprintf fmt "%a" (Printer.print_descr_ctx prec assoc) d

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params
