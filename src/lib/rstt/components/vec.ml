open Sstt
open Rstt_utils

let tag = Tag.mk' "v" (Tag.Monotonic {preserves_cap=true; preserves_cup=false ; preserves_extremum=true})
let prim_int = Prim.mk Prim.Int.any'
let mk ?(len=Ty.any) v =
  let ty = Descr.mk_tuple [Ty.cap v Prim.any ; Ty.cap len prim_int] |> Ty.mk_descr in
  TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let mk_len n v = mk ~len:(Prim.Int.int' n |> Prim.mk) v
(* let any_d = Descr.mk_tuple [ Prim.any ; prim_int ] |> Ty.mk_descr *)
let any = mk Ty.any

type 'a atom =
  | AnyLength of 'a
  | CstLength of int * 'a
  | VarLength of 'a * 'a
type 'a line = 'a atom * 'a atom list
type 'a t = 'a line list

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

let to_t node ctx comp =
  let dnf = TagComp.dnf comp in
  let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
  if Ty.leq ty any then Some (extract dnf |> map (node ctx))
  else None

let destruct ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> TagComp.dnf |> extract_pairs
let length ty =
  let l ((_,l),_) = l in
  destruct ty |> List.map l |> Ty.disj
let content ty =
  let v ((v,_),_) = v in
  destruct ty |> List.map v |> Ty.disj

let print prec assoc fmt t =
  let print_atom fmt = function
    | VarLength (l,v) ->
      let l = Utils.prune_printer_descr ~any:prim_int l in
      let v = Utils.prune_printer_descr ~any:Prim.any v in
      Format.fprintf fmt "%a[%a](%a)" Tag.pp tag
        Printer.print_descr l Printer.print_descr v
    | AnyLength v ->
      let v = Utils.prune_printer_descr ~any:Prim.any v in
      Format.fprintf fmt "%a(%a)" Tag.pp tag Printer.print_descr v
    | CstLength (n,v) ->
      let v = Utils.prune_printer_descr ~any:Prim.any v in
      Format.fprintf fmt "%a%i(%a)" Tag.pp tag n Printer.print_descr v
  in
  let print_atom_neg prec assoc fmt a =
    let sym,_,_ as opinfo = Prec.unop_info Neg in
    Prec.fprintf prec assoc opinfo fmt "%s%a" sym print_atom a
  in
  let print_line prec assoc fmt (a, ns) =
    if ns <> [] then
      let sym,prec',_ as opinfo = Prec.varop_info Cap in
      Prec.fprintf prec assoc opinfo fmt "%a%s%a"
        print_atom a sym (print_seq (print_atom_neg prec' NoAssoc) sym) ns
    else
      Format.fprintf fmt "%a" print_atom a
  in
  let sym,prec',_ as opinfo = Prec.varop_info Cup in
  Prec.fprintf prec assoc opinfo fmt "%a" (print_seq (print_line prec' NoAssoc) sym) t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:map ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
