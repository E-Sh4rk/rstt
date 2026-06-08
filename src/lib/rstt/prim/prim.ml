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
let any_p = [Int.any ; Chr.any ; Dbl.any ; Raw.any ; Clx.any ; Lgl.any] |> Ty.disj
let any_p' = [Int.any' ; Chr.any' ; Dbl.any' ; Raw.any' ; Clx.any' ; Lgl.any'] |> Ty.disj
let any = any_p |> add_tag
let any' = any_p' |> add_tag
let mk p = add_tag (Ty.cap p any_p)
let destruct p = proj_tag p
let comps =
  [ Int.any, Int.any_sub ; Chr.any, Chr.any_sub ; Dbl.any, Dbl.any_sub ;
    Raw.any, Raw.any_sub ; Clx.any, Clx.any_sub ; Lgl.any, Lgl.any_sub ]
let comps' =
  [ Int.any', Int.any_sub' ; Chr.any', Chr.any_sub' ; Dbl.any', Dbl.any_sub' ;
    Raw.any', Raw.any_sub' ; Clx.any', Clx.any_sub' ; Lgl.any', Lgl.any_sub' ]

type t = | TAny | TAny' | TComp of Printer.descr | TSubComp of Printer.descr
let is_sub pty =
  comps@comps' |> List.find_map (fun (any,any_sub) ->
      if Ty.equiv pty any_sub then Some (Ty.cap any pty) else None
    )
let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
  then
    if Ty.leq any_p pty then Some TAny
    else if Ty.equiv any_p' pty then Some TAny'
    else
      match is_sub pty with
      | None -> Some (TComp (ctx.Printer.build pty))
      | Some pty -> Some (TSubComp (ctx.Printer.build pty))
  else None
let map f = function TAny -> TAny | TAny' -> TAny'
  | TComp d -> TComp (f d) | TSubComp d -> TSubComp (f d)
let print prec assoc fmt t =
  let prim_ctx, suffix =
    match Pp.current_pos () with Prim s -> true, s | _ -> false, ""
  in
  match t with
  | TAny when prim_ctx -> Format.fprintf fmt "vec%s" suffix
  | TAny -> Format.fprintf fmt "prim"
  | TAny' when prim_ctx -> Format.fprintf fmt "%svec%s" Na.hat suffix
  | TAny' -> Format.fprintf fmt "%sprim" Na.hat
  | TComp d when prim_ctx -> Pp.print_descr_ctx prec assoc fmt d
  | TComp d -> Pp.pp_prim_tag Pp.print_descr_ctx prec assoc fmt d
  | TSubComp d ->
    let str = Format.asprintf "%a" (Pp.print_descr_ctx Prec.min_prec Prec.NoAssoc) d in
    let pp _ _ fmt () = Format.fprintf fmt "%s" (String.lowercase_ascii str) in
    if prim_ctx
    then pp prec assoc fmt ()
    else Pp.pp_prim_tag pp prec assoc fmt ()

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{ aliases = []; extensions = [tag, printer_builder]}
let () = Pp.add_printer_param printer_params

let partition = comps |> List.map (fun (any,_) -> mk any)
let simpl_comps =
  let (a1, a2), (a3, a4) = List.split comps, List.split comps' in
  List.concat [a1;a2;a3;a4] |> List.map mk
let is_simple ty =
  simpl_comps |> List.exists (Ty.equiv ty)
let is_singleton ty =
  Ty.vars_toplevel ty |> VarSet.is_empty &&
  let ty = proj_tag ty in
  comps |> List.exists (fun (any, _) -> Ty.leq ty any) &&
  (Int.is_singleton ty || Chr.is_singleton ty || Dbl.is_singleton ty ||
   Raw.is_singleton ty || Clx.is_singleton ty || Lgl.is_singleton ty)
