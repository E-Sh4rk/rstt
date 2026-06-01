open Sstt

module P = struct
  let tag = Tag.mk "s"
  let tag_name = "chr"

  let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
  let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                  |> Op.TagComp.as_atom |> snd

  let str str = Strings.enum str |> Descr.mk_enum |> Ty.mk_descr |> add_tag
  let any_p = Enums.any |> Descr.mk_enums |> Ty.mk_descr
  let var v = Ty.mk_var v |> Ty.cap any_p |> add_tag
  let any = add_tag any_p

  type t = string Utils.prim_t
  let any_t = Utils.any_prim_t
  let extract_line (pvs, nvs, d) =
    let pos, enums = d |> Descr.get_enums |> Enums.destruct in
    let strs = enums |> List.map Strings.string |> List.sort String.compare in
    { Utils.pos=pos ; prim=strs ; pvs ; nvs }
  let extract ty =
    Ty.def ty |> VDescr.dnf |> List.map extract_line

  let to_t _ ty =
    try
      let pty = proj_tag ty in
      if Ty.leq pty any_p
      then Some (extract pty)
      else None
    with Not_found -> None
  let destruct ty = proj_tag ty |> extract
  let is_singleton ty =
    let aux = Fun.const true in
    destruct ty |> Utils.is_singleton aux

  let map _ v = v

  let print prec assoc fmt lines =
    let pp_string _prec _assoc fmt str = Format.fprintf fmt "%S" str in
    let aux = Pp.print_cup ~cmp:String.compare pp_string in
    let dnf = Utils.t_to_dnf lines in
    let print_lit prec assoc fmt t =
      match t with
      | Utils.P (true, content) -> aux prec assoc fmt content
      | P (false, []) -> Format.fprintf fmt "chr"
      | P (false, content) -> Prec.print_binary_op' (Prec.print_atomic_str "chr") aux
          prec assoc Diff fmt () content
      | V v -> Format.fprintf fmt "chr(%a)" Var.pp v
    in
    Pp.print_non_empty_dnf ~any:"chr" ~cmp:Stdlib.compare print_lit prec assoc fmt dnf
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params

let str str = mk (P.str str)
let str' str = mk' (P.str str)
let var v = mk (P.var v)
let var' v = mk' (P.var v)
let destruct ty =
  match destruct ty with
  | Na -> true, []
  | WithNa ty -> true, P.destruct ty
  | WithoutNa ty -> false, P.destruct ty
