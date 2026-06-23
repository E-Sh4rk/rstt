open Sstt

module P = struct
  include Integer
  let tag_name = "num"

  let to_t _ = to_t ?pos:None

  let print prefix suffix prec assoc fmt lines =
    let any = prefix^"NUM"^suffix in
    let var = any in
    let pp_int fmt i = Format.fprintf fmt "%i" i in
    print ~any ~var ~pp_int prec assoc fmt lines
end

include Na.MakeCompWithNa(P)

let printer_builder = Printer.builder ~to_t ~map:(fun _ _ v -> v) ~print
let printer_params = Printer.{aliases =[]; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params

let proj_tag tag ty = ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag
                |> Op.TagComp.as_atom |> snd
let add_tag tag ty = (tag, ty) |> Descr.mk_tag |> Ty.mk_descr
let of_dbl ty =
  let ty = proj_tag Dbl.tag ty |> add_tag tag in
  if Ty.leq ty any then Some ty else None
let of_int ty =
  let ty = proj_tag Int.tag ty |> add_tag tag in
  if Ty.leq ty any then Some ty else None

(* Constructors *)
let int i = Ty.cup (Int.int i) (Dbl.int i)
let int' i = Ty.cup (Int.int' i) (Dbl.int' i)
let var v = Ty.cup (Int.var v) (Dbl.var v)
let var' v = Ty.cup (Int.var' v) (Dbl.var' v)
let interval i = Ty.cup (Int.interval i) (Dbl.interval i)
let interval' i = Ty.cup (Int.interval' i) (Dbl.interval' i)
let bounded i = Ty.cup (Int.bounded i) (Dbl.bounded i)
let bounded' i = Ty.cup (Int.bounded' i) (Dbl.bounded' i)
let any = Ty.cup Int.any (Dbl.interval (None, None))
let any' = Ty.cup Int.any' (Dbl.interval' (None, None))
