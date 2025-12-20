open Sstt

let prune_printer_descr ~any d =
  let rec aux d =
    let any_d = {Printer.ty=any ; op=Printer.Builtin Printer.Any} in
    if Ty.leq any d.Printer.ty then any_d else
      let op = match d.Printer.op with
      | Varop (Cap, lst) ->
        let lst =
          match List.filter (fun d -> Ty.leq any d.Printer.ty |> not) lst with
          | [] -> [any_d]
          | lst -> List.map aux lst
        in
        Printer.Varop (Cap, lst)
      | Varop (Cup, lst) -> Varop (Cup, List.map aux lst)
      | Binop (Diff, d1, d2) -> Binop (Diff, aux d1, d2)
      | op -> op
      in
      { Printer.op ; ty=d.ty }
    in
    aux d
