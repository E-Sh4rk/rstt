open Sstt

module P = struct
  include Integer
  let tag_name = "dbl"

  let others = Descr.mk_enum (Enum.mk "_") |> Ty.mk_descr
  let add_others ty = Ty.cup ty others

  (* let int i = int i |> add_others *)
  (* let interval i = interval i |> add_others *)
  (* let bounded i = bounded i |> add_others *)
  let var v = var v |> add_others
  let any = any |> add_others

  type t = { integers:Utils.interval Utils.atomic_t ; neg:bool }
  let any_t = { integers=[] ; neg=true }
  let may_not_feature_any { neg ; _ } = not neg
  let map f { integers ; neg } = { integers=map f integers ; neg }
  let to_t _ ty =
    let others = Ty.leq others ty in
    let ty = if others then Ty.diff Integer.any ty else ty in
    to_t ?pos:None ty |> Option.map (fun integers -> { neg=others ; integers })
    
  let destruct ty =
    let others = Ty.leq others ty in
    let ty = if others then Ty.diff Integer.any ty else ty in
    let integers = destruct ?pos:None ty in
    { neg=others ; integers }

  let print prefix suffix prec assoc fmt { integers ; neg } =
    let any = prefix^"DBL"^suffix in
    let var = any in
    let pp_int fmt i = Format.fprintf fmt "%i." i in
    let pp_integers = print ~any:"(..)" ~var ~pp_int in
    if neg && List.is_empty integers then
      Format.fprintf fmt "%s" any
    else if neg then
      Prec.print_binary_op' (Prec.print_atomic_str any) pp_integers
        prec assoc Diff fmt () integers
    else
      pp_integers prec assoc fmt integers
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params

type t = P.t = { integers:Utils.interval Utils.atomic_t ; neg:bool }
let int i = mk (P.int i)
let int' i = mk' (P.int i)
let var v = mk (P.var v)
let var' v = mk' (P.var v)
let interval i = mk (P.interval i)
let interval' i = mk' (P.interval i)
let bounded i = mk (P.bounded i)
let bounded' i = mk' (P.bounded i)
let destruct ty =
  match destruct ty with
  | Na -> true, { P.neg=false ; integers=[] }
  | WithNa ty -> true, P.destruct ty
  | WithoutNa ty -> false, P.destruct ty
let any_sub, any_sub' = Ty.cup any Int.any_sub, Ty.cup any' Int.any_sub'
