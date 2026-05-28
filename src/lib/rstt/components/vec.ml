open Sstt
module Reserved = Labels.Reserved

type 'a atom =
  | AnyLength of 'a
  | CstLength of int * 'a
  | VarLength of 'a * 'a
type 'a line = 'a atom * 'a atom list
type 'a t = 'a line list

let tag = Tag.mk "v"
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

let prim_int = Prim.mk Prim.Int.any'
let mk a =
  let open Records.Atom in
  let len, v =
    match a with
    | AnyLength c -> Ty.any, c
    | CstLength (n, c) -> Prim.Int.int' n |> Prim.mk, c
    | VarLength (l, c) -> l, c
  in
  let len, v = Ty.cap len prim_int, Ty.cap v Prim.any in
  let bindings = LabelMap.singleton Reserved.card (Ty.O.required len |> Ty.F.mk_descr) in
  let tail = Ty.O.required v |> Ty.F.mk_descr in
  Descr.mk_record { bindings ; tail } |> Ty.mk_descr |> add_tag
let any = mk (AnyLength Ty.any)

let map_atom f = function
  | AnyLength d -> AnyLength (f d)
  | CstLength (i,d) -> CstLength (i, f d)
  | VarLength (l,d) -> VarLength (f l, f d)
let map_line f (p,ns) = (map_atom f p, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract atom =
  let open Records.Atom in
  let len = find Reserved.card atom |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  let v = atom.tail |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get in
  (v, len)
let extract ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid vector encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf |> List.map (fun (ps,ns) ->
      let pvs, plens = List.map extract ps |> List.split in
      let pv, plen = Ty.conj pvs, Ty.conj plens in
      let ns = List.map extract ns in
      (pv,plen), ns
  )
let pair_to_atom (v,l) =
  if Ty.leq prim_int l
  then AnyLength v
  else
    match Prim.destruct l |> Prim.Int.destruct with
    | false, [(Some n1, Some n2)] when Stdlib.Int.equal n1 n2 -> CstLength (n1, v)
    | _ -> VarLength (l, v)

let to_t ctx comp =
  let pty = Op.TagComp.as_atom comp |> snd in
  let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
  if Ty.leq ty any then
    Some (extract pty |> List.map (fun (p, ns) -> pair_to_atom p, List.map pair_to_atom ns)
      |> map ctx.Printer.build)
  else None

let destruct ty =
  ty |> proj_tag |> extract
  |> List.map (fun (p, ns) -> pair_to_atom p, List.map pair_to_atom ns)

let partition =
  Prim.partition |> List.map (fun ty -> mk (AnyLength ty))

let print prec assoc fmt t =
  let open Rstt_utils in
  let cmp v1 v2 =
    match v1, v2 with
    | VarLength (l1,v1), VarLength (l2,v2) ->
      Pp.Compare.descr l1 l2 |> ccmp Pp.Compare.descr v1 v2
    | AnyLength v1, AnyLength v2 -> Pp.Compare.descr v1 v2
    | CstLength (n1,v1), CstLength (n2,v2) ->
      Stdlib.compare n1 n2 |> ccmp Pp.Compare.descr v1 v2
    | VarLength _, _ -> -1 | _, VarLength _ -> 1
    | AnyLength _, _ -> -1 | _, AnyLength _ -> 1
  in
  let print_prim_descr = Pp.print_prim_descr_ctx Prec.min_prec Prec.NoAssoc in
  let shortcut_v v =
    let str = Format.asprintf "%a" print_prim_descr v in
    let prefix = Format.asprintf "%(%)" (Na.Hat.sym ()) in
    if String.starts_with ~prefix str
    then
      let n = String.length prefix in
      String.sub str n (String.length str - n)
    else str
  in
  let print_v ~len fmt v =
    if Ty.leq Prim.any v.Printer.ty then
      Format.fprintf fmt "vec%s" len
    else if Ty.equiv Prim.any' v.ty then
      Format.fprintf fmt "%(%)vec%s" (Na.Hat.sym ()) len
    else if Prim.is_simple v.ty then
      Format.fprintf fmt "%a%s" print_prim_descr v len
    else
      let v = Utils.prune_printer_descr ~any:Prim.any v in
      Format.fprintf fmt "%a%s(%a)" Tag.pp tag len print_prim_descr v
  in
  let print_atom _prec _assoc fmt = function
    | VarLength (l,v) ->
      let l = Utils.prune_printer_descr ~any:prim_int l in
      let len = Format.asprintf "@[<h>[%a]@]" print_prim_descr l in
      Format.fprintf fmt "%a" (print_v ~len) v
    | AnyLength v ->
      Format.fprintf fmt "%a" (print_v ~len:"") v
    | CstLength (n,v) ->
      if n=1 && Prim.is_singleton v.Printer.ty then
        Format.fprintf fmt "%s" (shortcut_v v)
      else
        let len = Format.asprintf "%i" n in
        Format.fprintf fmt "%a" (print_v ~len) v
  in
  let t = t |> List.map (fun (p,ns) -> [p],ns) in
  Pp.print_non_empty_dnf ~cmp ~any:"" print_atom prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:map ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
