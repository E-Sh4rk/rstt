open Sstt
open Rstt_utils

module P = struct
  let tag_name = "int"

  type interval = int option * int option

  let int i =
    i |> Z.of_int |> Intervals.Atom.mk_singl
    |> Descr.mk_interval |> Ty.mk_descr

  let interval (i1, i2) =
    let lb, ub = i1 |> Option.map Z.of_int, i2 |> Option.map Z.of_int in
    Intervals.Atom.mk lb ub |> Descr.mk_interval |> Ty.mk_descr
  let bounded (i1, i2) = interval (Some i1, Some i2)

  let any =
    Intervals.Atom.mk None None
    |> Descr.mk_interval |> Ty.mk_descr

  let destruct ty =
    Ty.get_descr ty |> Descr.get_intervals |> Intervals.destruct
    |> List.map (fun a ->
              let i1, i2 = Intervals.Atom.get a in
              i1 |> Option.map Z.to_int, i2 |> Option.map Z.to_int)

  type t = (int option * int option) list
  let any_t = [None,None]

  let to_t _ ty =
    if Ty.leq ty any then Some (destruct ty) else None

  open Prec
  let map _f v = v
  let print_interval fmt (lb,ub) =
    match lb, ub with
    | None, None -> Format.fprintf fmt "int"
    | Some lb, Some ub when Stdlib.Int.equal lb ub ->
      Format.fprintf fmt "%i" lb
    | Some lb, Some ub ->
      Format.fprintf fmt "(%i..%i)" lb ub
    | None, Some ub ->
      Format.fprintf fmt "(..%i)" ub
    | Some lb, None ->
      Format.fprintf fmt "(%i..)" lb
  let print prec assoc fmt ints =
    match ints with
    | [] -> assert false
    | [i] -> Format.fprintf fmt "%a" print_interval i
    | ints ->
      let sym,_,_ as opinfo = varop_info Cup in
      fprintf prec assoc opinfo fmt "%a" (print_seq print_interval sym) ints
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params

type interval = P.interval
let int i = mk (P.int i)
let int' i = mk' (P.int i)
let interval i = mk (P.interval i)
let interval' i = mk' (P.interval i)
let bounded i = mk (P.bounded i)
let bounded' i = mk' (P.bounded i)
let destruct ty =
  match destruct ty with
  | Na -> true, []
  | WithNa ty -> true, P.destruct ty
  | WithoutNa ty -> false, P.destruct ty
