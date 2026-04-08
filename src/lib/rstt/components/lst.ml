open Sstt
open Rstt_utils

let tag = Tag.mk "lst"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

type 'a atom = { pos:'a list ; named:(string * 'a) list ; sym:(Labels.t list * 'a) list ; tl:'a }
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

let reserved_labels = [Labels.id ; Labels.npos]
let reserved_bindings = reserved_labels |> List.map (fun lbl -> lbl, Ty.O.absent |> Ty.F.mk_descr)
let mk { pos ; named ; sym ; tl } =
  let pos = List.mapi (fun i ty -> Labels.pos i, ty) pos in
  let named = List.map (fun (str,ty) -> Labels.named str, ty) named in
  let sym = List.map (fun (str,ty) -> Labels.sym str, ty) sym in
  let bindings = LabelMap.of_list (reserved_bindings@pos@named@sym) in
  let tail = Utils.add_option tl in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let any = mk {pos=[]; named=[]; sym=[]; tl=Ty.F.any}
let any_d = proj_tag any

let map_atom f {pos;named;sym;tl} =
  let aux (str,t) = str, f t in
  {pos=List.map f pos ; named=List.map aux named ; sym=List.map aux sym ; tl=f tl }
let map_line f (ps,ns) = (List.map (map_atom f) ps, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid list encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom { Records.Atom.bindings ; tail } =
  let rec partition lst =
    match lst with
    | [] -> [], [], []
    | (lbl,ty)::lst ->
      begin match Labels.info lbl with
    | Pos i ->
      let pos, named, sym = partition lst in
      (i,ty)::pos, named, sym
    | Named str ->
      let pos, named, sym = partition lst in
      pos, (str,ty)::named, sym
    | Sym str ->
      let pos, named, sym = partition lst in
      pos, named, (str,ty)::sym
    end
  in
  let pos, named, sym = bindings |> LabelMap.bindings
  |> List.filter (fun (lbl,_) -> reserved_labels |> List.exists (Label.equal lbl) |> not)
  |> partition
  in
  let pos = List.sort (fun t1 t2 -> Stdlib.compare (fst t1) (fst t2)) pos |> List.map snd in
  { pos ; named ; sym ; tl=tail }
let extract t : Ty.F.t t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_fop)
  else None

let destruct ty = proj_tag ty |> extract

let print prec assoc fmt t =
  let print_atom _prec _assoc fmt {pos;named;sym;tl} =
    let print_field_ty = Printer.print_field_ctx Prec.min_prec Prec.NoAssoc in
    let print_field fmt (name,ty) =
      match name with
      | None -> Format.fprintf fmt "%a" print_field_ty ty
      | Some str -> Format.fprintf fmt "%s: %a" str print_field_ty ty
    in
    let print_tail fmt f =
      match f with
      | Printer.FTy (t, true) when Ty.leq t.Printer.ty Ty.empty -> ()
      | Printer.FTy (t, true) when Ty.leq Attr.any t.ty -> Format.fprintf fmt "... "
      | f -> Format.fprintf fmt "; %a " print_field_ty (Utils.prune_option_fop f)
    in
    let pos, named, sym =
      List.map (fun t -> None, t) pos,
      List.map (fun (str,t) -> Some str, t) named,
      List.map (fun (str,t) -> Some (Labels.name (Sym str)), t) sym
    in
    Format.fprintf fmt "{ %a %a}" (print_seq print_field ", ") (pos@named@sym) print_tail tl
  in
  Prec.print_non_empty_dnf ~any:"list" print_atom prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map (Printer.map_fop f)) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
