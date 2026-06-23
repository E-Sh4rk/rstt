open Sstt

let tag = Tag.mk "cint"
let na_enum = Enum.mk "cint_na"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

let interval (i1, i2) =
  let lb, ub = i1 |> Option.map Z.of_int, i2 |> Option.map Z.of_int in
  Intervals.Atom.mk lb ub |> Descr.mk_interval |> Ty.mk_descr |> add_tag
let singl i = interval (Some i,Some i)
let na = Descr.mk_enum na_enum |> Ty.mk_descr |> add_tag
let tt, ff = singl 1, singl 0
let bool = Ty.cup tt ff
let any_p = Intervals.any |> Descr.mk_intervals |> Ty.mk_descr
let na_p = Descr.mk_enum na_enum |> Ty.mk_descr
let any_na_p = Ty.cup any_p na_p
let any_na = add_tag any_na_p
let any = add_tag any_p
let var v = Ty.mk_var v |> Ty.cap any_p |> add_tag
let var_na v = Ty.mk_var v |> Ty.cap any_na_p |> add_tag

type elt = AnyNa | Any | Bool | Tt | Ff | Interval of Utils.interval | Singl of int

let to_t _ comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_na_p
  then
    let aux (i1,i2) =
      let ty = Intervals.Atom.mk i1 i2 |> Descr.mk_interval |> Ty.mk_descr |> add_tag in
      if Ty.equiv ty any then Any
      else if Ty.equiv ty tt then Tt
      else if Ty.equiv ty ff then Ff
      else if Ty.equiv ty bool then Bool
      else
        match i1, i2 with
        | None, None -> assert false
        | Some i1, Some i2 when Z.equal i1 i2 -> Singl (Z.to_int i1)
        | _ -> Interval (Option.map Z.to_int i1, Option.map Z.to_int i2)
    in
    let aux (pvs,nvs,d) =
      let ty = Ty.mk_descr d in
      if Ty.equiv ty any_na_p then { Utils.pos=true ; prim=[AnyNa] ; pvs ; nvs }
      else
        if Ty.leq na_p ty then
          let ints' = d |> Descr.get_intervals |> Intervals.destruct_neg
            |> List.map (fun a-> Intervals.Atom.get a |> aux) in
          { Utils.pos=false ; prim=ints' ; pvs ; nvs }
        else
          let ints = d |> Descr.get_intervals |> Intervals.destruct
            |> List.map (fun a-> Intervals.Atom.get a |> aux) in
          { Utils.pos=true ; prim=ints ; pvs ; nvs }
    in
    Some (Ty.def pty |> VDescr.dnf |> List.map aux)
  else None

let print prec assoc fmt lines =
  let pp_int fmt i = Format.fprintf fmt "%i" i in 
  let pp_interval _prec _assoc fmt i =
    match i with
    | AnyNa -> Format.fprintf fmt "c_int_na"
    | Any -> Format.fprintf fmt "c_int"
    (* | Na -> Format.fprintf fmt "c_na" *)
    | Bool -> Format.fprintf fmt "c_bool"
    | Tt -> Format.fprintf fmt "c_true"
    | Ff -> Format.fprintf fmt "c_false"
    | Singl i -> Format.fprintf fmt "c(%a)" pp_int i
    | Interval (i1,i2) -> Format.fprintf fmt "c%a"
      (Utils.print_interval "(..)" pp_int prec assoc) (i1,i2)
  in
  let aux = Pp.print_cup ~cmp:Stdlib.compare pp_interval in
  let dnf = Utils.t_to_dnf lines in
  let print_lit prec assoc fmt t =
    match t with
    | Utils.P (true, ints) -> aux prec assoc fmt ints
    | P (false, []) -> Format.fprintf fmt "c_int_na"
    | P (false, ints) -> Prec.print_binary_op' (Prec.print_atomic_str "c_int_na") aux
        prec assoc Diff fmt () ints
    | V v -> Format.fprintf fmt "c_int_na(%a)" Var.pp v
  in
  Pp.print_non_empty_dnf ~any:"c_int_na" ~cmp:Stdlib.compare print_lit prec assoc fmt dnf

let printer_builder = Printer.builder ~to_t ~map:(fun _ _ v -> v) ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
