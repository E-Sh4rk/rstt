open Sstt

module P = struct
  let tag = Tag.mk "i"
  let tag_name = "int"

  let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
  let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd

  let int i =
    i |> Z.of_int |> Intervals.Atom.mk_singl
    |> Descr.mk_interval |> Ty.mk_descr |> add_tag

  let interval (i1, i2) =
    let lb, ub = i1 |> Option.map Z.of_int, i2 |> Option.map Z.of_int in
    Intervals.Atom.mk lb ub |> Descr.mk_interval |> Ty.mk_descr |> add_tag
  let bounded (i1, i2) = interval (Some i1, Some i2)

  let any_p =
    Intervals.Atom.mk None None
    |> Descr.mk_interval |> Ty.mk_descr
  let var v = Ty.mk_var v |> Ty.cap any_p |> add_tag
  let any = any_p |> add_tag

  let conv_intervals intervals =
    intervals |> List.map (fun a ->
              let i1, i2 = Intervals.Atom.get a in
              i1 |> Option.map Z.to_int, i2 |> Option.map Z.to_int)
  let extract ty =
    let intervals = Ty.get_descr ty |> Descr.get_intervals |> Intervals.destruct in
    let intervals' = Ty.get_descr ty |> Descr.get_intervals |> Intervals.destruct_neg in
    if List.length intervals' < List.length intervals
    then false, conv_intervals intervals'
    else true, conv_intervals intervals

  type t = bool * (int option * int option) list
  let any_t = false, []

  let to_t _ ty =
    let pty = proj_tag ty in
    if Ty.leq pty any_p && (Ty.vars_toplevel pty |> VarSet.is_empty)
    then Some (extract pty)
    else None
  let destruct ty = proj_tag ty
    |> Ty.get_descr |> Descr.get_intervals |> Intervals.destruct |> conv_intervals
  let is_singleton ty =
    match destruct ty with
    | [(Some i1, Some i2)] -> Stdlib.Int.equal i1 i2
    | _ -> false

  let map _f v = v
  let print prec assoc fmt (pos,ints) =
    let aux = Pp.print_cup ~cmp:Stdlib.compare (Utils.print_interval "int") in
    if pos then
      aux prec assoc fmt ints
    else if not pos && ints = [] then
      Format.fprintf fmt "int"
    else
      Prec.print_binary_op' (Prec.print_atomic_str "int") aux
        prec assoc Diff fmt () ints
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params

let int i = mk (P.int i)
let int' i = mk' (P.int i)
let var v = mk' (P.var v)
let interval i = mk (P.interval i)
let interval' i = mk' (P.interval i)
let bounded i = mk (P.bounded i)
let bounded' i = mk' (P.bounded i)
let destruct ty =
  match destruct ty with
  | Na -> true, []
  | WithNa ty -> true, P.destruct ty
  | WithoutNa ty -> false, P.destruct ty
