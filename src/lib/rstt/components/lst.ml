open Sstt

let tag = Tag.mk "lst"
let mk pos named tail =
  let pos = List.mapi (fun i ty -> Labels.pos i, ty) pos in
  let named = List.map (fun (str,ty) -> Labels.named str, ty) named in
  let bindings = LabelMap.of_list (pos@named) in
  let ty = { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr in
  TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let any_d = Records.any |> Descr.mk_records |> Ty.mk_descr
let any = mk [] [] Ty.F.any

type 'a atom = 'a list * (string * 'a) list * 'a
type 'a line = 'a atom list * 'a atom list
type 'a t = 'a line list

let map_atom f (pos,named,tl) = List.map f pos, List.map (fun (str,t) -> str, f t) named, f tl
let map_line f (ps,ns) = (List.map (map_atom f) ps, List.map (map_atom f) ns)
let map f (l : 'a t) = l |> List.map (map_line f)

let extract_records ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid list encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom { Records.Atom.bindings ; tail } =
  let pos, named = bindings |> LabelMap.bindings |> List.partition_map (fun (lbl,ty) ->
    match Labels.info lbl with
    | Pos i -> Either.left (i,ty)
    | Named str -> Either.right (str,ty)
    | PosNamed _ -> invalid_arg "Invalid list encoding."
    ) in
  let pos = List.sort (fun t1 t2 -> Stdlib.compare (fst t1) (fst t2)) pos in
  let pos = List.map snd pos in
  pos, named, tail
let extract t : Ty.F.t t =
  extract_records t |> List.map
    (fun (ps, ns) -> List.map record_to_atom ps, List.map record_to_atom ns)
let to_t node ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map (node ctx))
  else None

let destruct ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd |> extract

(* TODO *)

(*
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
*)