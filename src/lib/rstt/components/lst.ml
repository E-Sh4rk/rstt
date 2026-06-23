open Sstt
open Rstt_utils

let tag = Tag.mk "lst"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type 'a atom = { bindings: (string * 'a) list ; sym: (Labels.sym * 'a) list ; tl:'a }
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

let mk { bindings ; sym ; tl } =
  let bindings = List.map (fun (str,ty) -> Labels.named str, ty) bindings in
  let sym = List.map (fun (str,ty) -> Labels.sym str, ty) sym in
  let bindings = LabelMap.of_list (bindings@sym) in
  let tail = Utils.add_option tl in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let any = mk {bindings=[]; sym=[]; tl=Ty.F.any}
let any_d = proj_tag any
let empty = mk {bindings=[]; sym=[]; tl=Ty.F.mk_descr Ty.O.absent}

let map_atom f {bindings;sym;tl} =
  let aux (str,t) = str, f t in
  { bindings=List.map aux bindings ; sym=List.map aux sym ; tl=f tl }
let map_line f (ps,ns) = (List.map (map_atom f) ps, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid list encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom { Records.Atom.bindings ; tail } =
  let rec partition lst =
    match lst with
    | [] -> [], []
    | (lbl,ty)::lst ->
      let named, sym = partition lst in
      begin match Labels.info lbl with
      | Pos _ -> assert false
      | Named str -> (str,ty)::named, sym
      | Sym str -> named, (str,ty)::sym
      end
  in
  let bindings, sym = bindings |> LabelMap.bindings |> partition in
  { bindings ; sym ; tl=tail }
let extract t : Ty.F.t t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_field)
  else None

let destruct ty = proj_tag ty |> Ty.cap any_d |> extract

let print prec assoc fmt t =
  let cmp {bindings=b1;sym=s1;tl=t1} {bindings=b2;sym=s2;tl=t2} =
    let open Rstt_utils in
    let cmp_field (str1,f1) (str2,f2) =
      String.compare str1 str2 |> ccmp Pp.Compare.fdescr f1 f2
    in
    let cmp_sym_field (lbl1,f1) (lbl2,f2) =
      Stdlib.compare lbl1 lbl2 |> ccmp Pp.Compare.fdescr f1 f2
    in
    let cmp_bindings b1 b2 = List.compare cmp_field b1 b2 in
    let cmp_sym_bindings b1 b2 = List.compare cmp_sym_field b1 b2 in
    Pp.Compare.fdescr t1 t2 |> ccmp cmp_sym_bindings s1 s2
    |> ccmp cmp_bindings b1 b2
  in
  let is_absent fd = Ty.F.equiv fd.Printer.fty (Ty.F.mk_descr Ty.O.absent) in
  let print_field_ty fmt f =
    match f with
    | f when is_absent f -> Format.fprintf fmt "absent"
    | f -> Printer.print_field_ctx Prec.min_prec Prec.NoAssoc fmt f
  in
  let print_field suffix fmt (str,ty) =
    Format.fprintf fmt "%s: %a%s" str print_field_ty ty suffix
  in
  let print_atom _prec _assoc fmt {bindings;sym;tl} =
    let sym = List.map (fun (str,t) -> Labels.name (Sym str), t) sym in
    match tl with
    | tl when is_absent tl ->
      Format.fprintf fmt "{ %a }" (print_seq (print_field "") ", ") (bindings@sym)
    | _ ->
      Format.fprintf fmt "{ %a%a }" (print_seq (print_field ", ") "") (bindings@sym)
        print_field_ty (Utils.prune_option_fdescr tl)
  in
  Pp.print_non_empty_dnf ~cmp ~any:"list" print_atom prec assoc fmt t
let print = Utils.struct_print print

let printer_builder =
  Printer.builder
    ~to_t:to_t
    ~map:(fun f -> map (Printer.map_fdescr (fun d -> (f d).op) (fun fd -> fd.fop)))
    ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
