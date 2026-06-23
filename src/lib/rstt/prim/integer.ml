open Sstt

let tag = Tag.mk "i"
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
let extract_line ?pos (pvs, nvs, d) =
  let intervals = d |> Descr.get_intervals |> Intervals.destruct in
  let intervals' = d |> Descr.get_intervals |> Intervals.destruct_neg in
  let pos = match pos with
  | Some b -> b
  | None -> List.length intervals' >= List.length intervals
  in
  if pos
  then { Utils.pos=true ; prim=conv_intervals intervals ; pvs ; nvs }
  else { Utils.pos=false ; prim=conv_intervals intervals' ; pvs ; nvs }
let extract ?pos ty =
  Ty.def ty |> VDescr.dnf |> List.map (extract_line ?pos)

let any_t = Utils.any_atomic_t

type t = Utils.interval Utils.atomic_t
let to_t ?pos ty =
  let pty = proj_tag ty in
  if Ty.leq pty any_p
  then Some (extract ?pos pty)
  else None
let destruct ?pos ty = proj_tag ty |> extract ?pos
let is_singleton ty =
  let aux = function (Some i1, Some i2) -> Stdlib.Int.equal i1 i2 | _ -> false in
  destruct ty |> Utils.is_singleton aux
let may_not_feature_any t =
  let aux = function (None, None) -> false | _ -> true in
  Utils.may_not_feature_any aux t

let print ~any ~var ~pp_int prec assoc fmt lines =
  let aux = Pp.print_cup ~cmp:Stdlib.compare
    (Utils.print_interval any pp_int) in
  let dnf = Utils.t_to_dnf lines in
  let print_lit prec assoc fmt t =
    match t with
    | Utils.P (true, ints) -> aux prec assoc fmt ints
    | P (false, []) -> Format.fprintf fmt "%s" any
    | P (false, ints) -> Prec.print_binary_op' (Prec.print_atomic_str any) aux
        prec assoc Diff fmt () ints
    | V v -> Format.fprintf fmt "%s(%a)" var Var.pp v
  in
  Pp.print_non_empty_dnf ~any ~cmp:Stdlib.compare print_lit prec assoc fmt dnf
