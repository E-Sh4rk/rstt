open Sstt
module Reserved = Labels.Reserved

type 'a atom =
  | Vector of 'a
  | Scalar of 'a
type 'a line = 'a atom * 'a atom list
type 'a t = 'a line list

let tag = Tag.mk "v"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

(* TODO: forbid tl type variables, enlarge prim for vectors *)
(* TODO: encoding such that v(a|b) = v(a)|v(b) *)
let mk a =
  let open Records.Atom in
  let elt0, tail =
  match a with
  | Vector c ->
    let c = Ty.cap c Prim.any in
    let elt0 = Ty.O.required c |> Ty.F.mk_descr in
    let tail = Ty.O.optional c |> Ty.F.mk_descr in
    elt0, tail
  | Scalar c ->
    let c = Ty.cap c Prim.any in
    let elt0 = Ty.O.required c |> Ty.F.mk_descr in
    let tail = Ty.O.absent |> Ty.F.mk_descr in
    elt0, tail
  in
  let bindings = LabelMap.singleton Reserved.elt0 elt0 in
  Descr.mk_record { bindings ; tail } |> Ty.mk_descr |> add_tag
let mk_line (p, ns) =
  let p = mk p in
  let ns = List.map mk ns |> List.map Ty.neg in
  Ty.conj (p::ns)
let any = mk (Vector Ty.any)

let map_atom f = function
  | Vector c -> Vector (f c)
  | Scalar c -> Scalar (f c)
let map_line f (p,ns) = (map_atom f p, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract atom =
  let open Records.Atom in
  let elt0 = find Reserved.elt0 atom |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  let tail = atom.tail |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  if Ty.is_empty tail
  then Scalar elt0 else Vector elt0
let merge_atoms a1 a2 =
  match a1, a2 with
  | Vector c1, Scalar c2 | Scalar c1, Vector c2 | Scalar c1, Scalar c2
  -> Scalar (Ty.cap c1 c2)
  | Vector c1, Vector c2 -> Vector (Ty.cap c1 c2)
let extract ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid vector encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf |> List.map (fun (ps,ns) ->
      let p = List.map extract ps |> List.fold_left merge_atoms (Vector Ty.any) in
      let ns = List.map extract ns in
      p, ns
  )
let to_t ctx comp =
  let pty = Op.TagComp.as_atom comp |> snd in
  let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
  if Ty.leq ty any
  then Some (extract pty |> map ctx.Printer.build)
  else None

let destruct ty =
  ty |> proj_tag |> extract

let partition =
  Prim.partition |> List.map (fun ty -> mk (Vector ty))

let print prec assoc fmt t =
  let cmp v1 v2 =
    match v1, v2 with
    | Vector v1, Vector v2 -> Pp.Compare.descr v1 v2
    | Scalar v1, Scalar v2 -> Pp.Compare.descr v1 v2
    | Vector _, _ -> -1 | _, Vector _ -> 1
  in
  let print_vec = Pp.print_descr_ctx' (Prim "") in
  let print_scalar = Pp.print_descr_ctx' (Prim "1") in
  let print_atom prec assoc fmt = function
    | Vector v -> print_vec prec assoc fmt v
    | Scalar v -> print_scalar prec assoc fmt v
  in
  let t = t |> List.map (fun (p,ns) -> [p],ns) in
  Pp.print_non_empty_dnf ~cmp ~any:"vec" print_atom prec assoc fmt t
let print = Utils.struct_print print

let printer_builder =
  Printer.builder ~to_t:to_t ~map:map ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
