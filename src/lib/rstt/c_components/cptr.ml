open Sstt
module Reserved = Labels.Reserved

module PtrStar = struct
  let sym () = format_of_string "*"
  let prec = 5
  let assoc = Prec.NoAssoc
  let opinfo () = (sym (), prec, assoc)
end

let tag = Tag.mk "cptr"
let add_tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let proj_tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                |> Op.TagComp.as_atom |> snd
let mk' ty =
  let open Records.Atom in
  let bindings =  [Reserved.target, ty |> Ty.O.optional |> Ty.F.mk_descr] |> LabelMap.of_list in
  let ty = Descr.mk_record { bindings ; tail=Ty.F.any } |> Ty.mk_descr in
  add_tag ty
let any = mk' Ty.any
let null = mk' Ty.empty
let string = mk' Cstring.any
let var_string v = Ty.diff (mk' (Cstring.var v)) null
let singl_string str = Ty.diff (mk' (Cstring.singl str)) null
let mk_nonstring ty = mk' (Ty.diff ty Cstring.any)

type 'a t = { nullable:bool ; target:'a ; str:'a }
let map f { nullable ; target; str } = { nullable ; target=f target ; str=f str }
let extract ty =
  let oty = Ty.get_descr ty |> Descr.get_records |> Op.Records.approx
  |> Op.Records.Atom.find Reserved.target |> Ty.O.get in
  let nullable, target = Ty.O.Atom.is_optional oty, Ty.O.Atom.get oty in
  let target, str = Ty.diff target Cstring.any, Ty.cap target Cstring.any in
  { nullable ; target ; str }

let to_t ctx comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  Some (extract pty |> map ctx.Printer.build)
let destruct ty = ty |> proj_tag |> extract
let print prec assoc fmt { nullable ; target ; str } =
  let pp_target prec assoc fmt target =
    Prec.print_unary Pp.print_descr_ctx prec assoc (PtrStar.opinfo ()) fmt
      (Utils.prune_printer_descr ~any:(Ty.neg Cstring.any) target)
  in
  let pp_str = Pp.print_descr_ctx in
  let pp_target_str prec assoc fmt (target,str) =
    let target_is_empty, str_is_empty = Ty.is_empty target.Printer.ty, Ty.is_empty str.Printer.ty in
    if target_is_empty && str_is_empty then Format.fprintf fmt "c_null"
    else if target_is_empty then pp_str prec assoc fmt str
    else if str_is_empty then pp_target prec assoc fmt target
    else if Ty.is_any (Ty.cup target.Printer.ty str.Printer.ty)
    then Format.fprintf fmt "c_ptr"
    else
      let sym,prec',_ as opinfo = Prec.varop_info Cup in
      Prec.fprintf prec assoc opinfo fmt "%a%(%)%a"
        (pp_target prec' Left) target sym (pp_str prec' Right) str
  in
  if nullable then
    pp_target_str prec assoc fmt (target,str)
  else
    Prec.print_binary_op' pp_target_str (Prec.print_atomic_str "c_null")
      prec assoc Diff fmt (target,str) ()


let printer_builder = Printer.builder ~to_t ~map ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
