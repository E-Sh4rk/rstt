open Sstt

type strings = { positive : bool ; content : string list }

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

  type t = strings
  let any_t = { positive=false ; content=[] }
  let extract ty =
    let (pos, enums) = ty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct in
    let strs = enums |> List.map Strings.string in
    { positive=pos ; content=strs }

  let to_t _ ty =
    try
      let pty = proj_tag ty in
      if Ty.leq pty any_p && Ty.vars_toplevel pty |> VarSet.is_empty
      then Some (extract pty)
      else None
    with Not_found -> None
  let destruct ty = proj_tag ty |> extract
  let is_singleton ty =
    match destruct ty with
    | { positive=true ; content=[_] } -> true
    | _ -> false
  let map _ v = v

  let print prec assoc fmt { positive ; content } =
    let pp_string _prec _assoc fmt str = Format.fprintf fmt "%S" str in
    let aux = Pp.print_cup ~cmp:String.compare pp_string in
    if positive then
      aux prec assoc fmt content
    else if not positive && content = [] then
      Format.fprintf fmt "chr"
    else
      Prec.print_binary_op' (Prec.print_atomic_str "chr") aux
        prec assoc Diff fmt () content
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
  | Na -> true, { positive=true ; content=[] }
  | WithNa ty -> true, P.destruct ty
  | WithoutNa ty -> false, P.destruct ty
