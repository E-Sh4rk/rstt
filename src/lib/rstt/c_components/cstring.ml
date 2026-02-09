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
    if Ty.leq pty any_p && Ty.vars_toplevel pty |> VarSet.is_empty then
      let (pos, enums) = pty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
      let strs = enums |> List.map Strings.string in
      Some (pos, strs)
    else
      None
  with Not_found -> None

let map _ v = v
  open Prec

let print prec assoc fmt (pos, strs) =
  let pp_string _prec _assoc fmt str = Format.fprintf fmt "c(%S)" str in
  let aux = print_cup pp_string in
  if pos then
    aux prec assoc fmt strs
  else if not pos && strs = [] then
    Format.fprintf fmt "c_string"
  else
    let sym,prec',_ as opinfo = binop_info Diff in
    fprintf prec assoc opinfo fmt "c_string%(%)%a" sym (aux prec' Right) strs

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
