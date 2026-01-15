open Sstt

type 'a atom =
  | AnyLength of 'a
  | CstLength of int * 'a
  | VarLength of 'a * 'a
type 'a line = 'a atom * 'a atom list
type 'a t = 'a line list

let tag = Tag.mk' "v" (Tag.Monotonic {preserves_cap=true; preserves_cup=false ; preserves_extremum=true})
let prim_int = Prim.mk Prim.Int.any'
let mk a =
  let len, v =
    match a with
    | AnyLength c -> Ty.any, c
    | CstLength (n, c) -> Prim.Int.int' n |> Prim.mk, c
    | VarLength (l, c) -> l, c
  in
  let ty = Descr.mk_tuple [Ty.cap v Prim.any ; Ty.cap len prim_int] |> Ty.mk_descr in
  TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let any = mk (AnyLength Ty.any)

let map_atom f = function
  | AnyLength d -> AnyLength (f d)
  | CstLength (i,d) -> CstLength (i, f d)
  | VarLength (l,d) -> VarLength (f l, f d)
let map_line f (p,ns) = (map_atom f p, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract_pair (_,ty) =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid vector encoding." ; 
  Ty.get_descr ty |> Descr.get_tuples |> Tuples.get 2 |>
  Op.TupleComp.approx |> (function [a;b] -> a,b | _ -> assert false)
let extract_pairs dnf =
  dnf |> List.map (fun (ps, ns) ->
    let vs,ls = ps |> List.map extract_pair |> List.split in
    let p = (Ty.conj vs, Ty.conj ls) in
    let ns = ns |> List.map extract_pair in
    p, ns
  )
let pair_to_atom (v,l) =
  if Ty.leq prim_int l
  then AnyLength v
  else
    match Prim.destruct l |> Prim.Int.destruct with
    | false, [(Some n1, Some n2)] when Stdlib.Int.equal n1 n2 -> CstLength (n1, v)
    | _ -> VarLength (l, v)
let extract dnf =
  dnf |> extract_pairs |> List.map (fun (p, ns) -> pair_to_atom p, List.map pair_to_atom ns)

let to_t ctx comp =
  let dnf = TagComp.dnf comp in
  let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
  if Ty.leq ty any then Some (extract dnf |> map ctx.Printer.build)
  else None

let destruct ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> TagComp.dnf |> extract_pairs
  |> List.map (fun (p, ns) -> pair_to_atom p, List.map pair_to_atom ns)

let partition =
  Prim.partition |> List.map (fun ty -> mk (AnyLength ty))

let print prec assoc fmt t =
  let print_v ~len fmt v =
    if Ty.leq Prim.any v.Printer.ty then
      Format.fprintf fmt "vec%s" len
    else if Ty.equiv Prim.any' v.ty then
      Format.fprintf fmt "%(%)vec%s" (Na.Hat.sym ()) len
    else if Prim.is_simple v.ty then
      Format.fprintf fmt "%a%s" Printer.print_descr v len
    else
      let v = Utils.prune_printer_descr ~any:Prim.any v in
      Format.fprintf fmt "%a%s(%a)" Tag.pp tag len Printer.print_descr v
  in
  let print_atom _prec _assoc fmt = function
    | VarLength (l,v) ->
      let l = Utils.prune_printer_descr ~any:prim_int l in
      let len = Format.asprintf "@[<h>[%a]@]" Printer.print_descr l in
      Format.fprintf fmt "%a" (print_v ~len) v
    | AnyLength v ->
      Format.fprintf fmt "%a" (print_v ~len:"") v
    | CstLength (n,v) ->
      let len = Format.asprintf "%i" n in
      Format.fprintf fmt "%a" (print_v ~len) v
  in
  let t = t |> List.map (fun (p,ns) -> [p],ns) in
  Prec.print_non_empty_dnf ~any:"" print_atom prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:map ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
