open Sstt

let tag = Tag.mk "cstring"

let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr

let singl str = Strings.enum str |> Descr.mk_enum |> Ty.mk_descr |> add_tag
let any_p = Enums.any |> Descr.mk_enums |> Ty.mk_descr
let var v = Ty.mk_var v |> Ty.cap any_p |> add_tag
let any = add_tag any_p

let to_t _ comp =
  try
    let (_, pty) = Op.TagComp.as_atom comp in
    if Ty.leq pty any_p
    then
      let aux (pvs,nvs,d) =
        let (pos, enums) = d |> Descr.get_enums |> Enums.destruct in
        let strs = enums |> List.map Strings.string |> List.sort String.compare in
        { Utils.pos ; prim=strs ; pvs ; nvs }
      in
      Some (Ty.def pty |> VDescr.dnf |> List.map aux)
    else None
  with Not_found -> None

let map _ v = v

let print prec assoc fmt lines =
  let pp_string _prec _assoc fmt str = Format.fprintf fmt "c(%S)" str in
  let aux = Pp.print_cup ~cmp:String.compare pp_string in
  let dnf = Utils.t_to_dnf lines in
  let print_lit prec assoc fmt t =
    match t with
    | Utils.P (true, strs) -> aux prec assoc fmt strs
    | P (false, []) -> Format.fprintf fmt "c_string"
    | P (false, strs) -> Prec.print_binary_op' (Prec.print_atomic_str "c_string") aux
        prec assoc Diff fmt () strs
    | V v -> Format.fprintf fmt "c_string(%a)" Var.pp v
  in
  Pp.print_non_empty_dnf ~any:"c_string" ~cmp:Stdlib.compare print_lit prec assoc fmt dnf

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
