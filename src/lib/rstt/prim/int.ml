open Sstt
open Rstt_utils

let tag = Tag.mk "int"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

type interval = int option * int option

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
let any = add_tag any_p

let to_t _ _ comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && Ty.vars_toplevel pty |> VarSet.is_empty
  then
    Some (pty |> Ty.get_descr |> Descr.get_intervals |> Intervals.destruct
          |> List.map (fun a ->
            let i1, i2 = Intervals.Atom.get a in
            i1 |> Option.map Z.to_int, i2 |> Option.map Z.to_int))
  else None

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

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
