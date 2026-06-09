open Sstt
module Reserved = Labels.Reserved

let tag = Tag.mk "attr"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type ('a, 'c) atom = { content:'a ; classes:'c ; attrs:'a }
type ('a, 'c) line = ('a, 'c) atom list * ('a, 'c) atom list
type ('a, 'c) t = ('a, 'c) line list


let mk { content ; classes ; attrs } =
  let classes = Ty.cap classes Classes.any in
  let attrs = Ty.cap attrs Lst.any in
  let bindings = LabelMap.of_list [
    Reserved.content, Ty.F.mk_descr (Ty.O.required content) ;
    Reserved.classes, Ty.F.mk_descr (Ty.O.required classes) ;
    Reserved.attrs, Ty.F.mk_descr (Ty.O.required attrs) ] in
  { Records.Atom.bindings ; tail=Ty.F.any } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk_content content =
  mk { content ; classes=Classes.any ; attrs=Lst.any }
let mk_content_noattr content =
  mk { content ; classes=Classes.noclass ; attrs=Lst.empty }
let any_d =
  let bindings = LabelMap.of_list [
    Reserved.content, Ty.F.mk_descr (Ty.O.required Ty.any) ;
    Reserved.classes, Ty.F.mk_descr (Ty.O.required Classes.any) ;
    Reserved.attrs, Ty.F.mk_descr (Ty.O.required Lst.any) ] in
  { Records.Atom.bindings ; tail=Ty.F.any } |> Descr.mk_record |> Ty.mk_descr
let any = add_tag any_d
let mk_line (ps,ns) =
  let ps = List.map mk ps in
  let ns = List.map mk ns |> List.map Ty.neg in
  Ty.conj (any::ps@ns)

let map_atom f fc { content ; classes ; attrs } = { content=f content ; classes=fc classes ; attrs=f attrs }
let map_line f fc (ps,ns) = (List.map (map_atom f fc) ps, List.map (map_atom f fc) ns)
let map f fc (l : ('a,'b) t) = l |> List.map (map_line f fc)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid attr encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom r =
  let content = Records.Atom.find Reserved.content r |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  let classes = Records.Atom.find Reserved.classes r  |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  let attrs = Records.Atom.find Reserved.attrs r  |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  { content ; classes ; attrs }
let extract t : (Ty.t, Ty.t) t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build ctx.Printer.build)
  else None

let destruct ty = proj_tag ty |> extract

let proj lbl ty =
  proj_tag ty |> Ty.get_descr |> Descr.get_records |> Op.Records.proj lbl |> Ty.O.get |> Ty.O.Atom.get

let proj_content = proj Reserved.content
let proj_classes = proj Reserved.classes
let proj_attrs = proj Reserved.attrs

let attr_content =
  [ Sstt.Arrows.any |> Sstt.Descr.mk_arrows |> Sstt.Ty.mk_descr ;
    Env.any ; ExternalPtr.any ; Lang.any ; Lst.any ; Vec.any ]
  |> Ty.disj
let is_descr_attr_content {Sstt.Printer.ty ; _} =
  Ty.leq ty attr_content
let print prec assoc fmt t =
  let cmp { content=c1 ; classes=cl1 ; attrs=a1 }
          { content=c2 ; classes=cl2 ; attrs=a2 } =
    Pp.Compare.descr c1 c2 |> Rstt_utils.ccmp Pp.Compare.descr cl1 cl2
    |> Rstt_utils.ccmp Pp.Compare.descr a1 a2
  in
  let print_atom prec assoc fmt { content ; classes ; attrs } =
    let print_opt_content fmt t =
      if Ty.is_any t.Printer.ty |> not
      then Pp.print_struct_descr_ctx Prec.max_prec Prec.NoAssoc fmt t
    in
    let print_content fmt t =
      Pp.print_struct_descr_ctx Prec.max_prec Prec.NoAssoc fmt t
    in
    let print_classes fmt t = Pp.print_descr_atomic fmt t in
    let print_attrs fmt t =
      Pp.print_struct_descr_ctx Prec.max_prec Prec.NoAssoc fmt t
    in
    let anyclass = Ty.leq Classes.any classes.Printer.ty in
    let anyattr = Ty.leq Lst.any attrs.Printer.ty in
    if anyclass && anyattr && is_descr_attr_content content then
      Format.fprintf fmt "%a" (Pp.print_struct_descr_ctx prec assoc) content
    else if anyattr then
      Format.fprintf fmt "%a%a" print_opt_content content print_classes classes
    else if anyclass then
      Format.fprintf fmt "%a with %a" print_content content print_attrs attrs
    else
      Format.fprintf fmt "%a%a with %a"
        print_opt_content content print_classes classes print_attrs attrs
  in
  Pp.print_non_empty_dnf ~cmp ~any:"attr" print_atom prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map f f) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
