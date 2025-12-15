open Sstt
(* open Rstt_utils *)

let tag = Tag.mk "arg"
type 'a t = { pos : 'a list ; pos_named : (string * 'a) list ; tl : 'a ; named : (string * 'a) list }
let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd

let mk _t = failwith "TODO"
let any_d = Ty.any (* TODO *)
let any = add_tag any_d

let map _f _t = failwith "TODO"

let extract_record ty =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid arg encoding." ; 
  Ty.get_descr ty |> Descr.get_records |> Records.dnf
let record_to_atom _r =
  failwith "TODO"
let extract t : Ty.F.t t =
  extract_record t |> record_to_atom
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_fop)
  else None

let destruct ty =
  proj_tag ty |> extract

let print _prec _assoc _fmt _t = failwith "TODO"

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map (Printer.map_fop f)) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
