open Sstt

let any = Lst.mk {
  Lst.bindings=[] ;
  Lst.tl=Ty.cup Sym.any (Attr.mk_content Lang.any) |> Ty.O.optional |> Ty.F.mk_descr
  }
let printer_params = Printer.{ aliases = [any,"expr"]; extensions = []}
let () = Pp.add_printer_param printer_params
