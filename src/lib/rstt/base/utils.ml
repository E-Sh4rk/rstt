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

let prune_option_fop fop =
  let rec aux = function
  | Printer.FTy (ty,_) -> Printer.FTy (ty,false)
  | FVarop (o,es) -> FVarop (o, List.map aux es)
  | FBinop (o,e1,e2) -> FBinop (o, aux e1, aux e2)
  | FUnop (o,e) -> FUnop (o, aux e)
  | FRowVar v -> FRowVar v
  in
  aux fop

type interval = int option * int option
let print_interval any _prec _assoc fmt (lb,ub) =
  match lb, ub with
  | None, None -> Format.fprintf fmt "%s" any
  | Some lb, Some ub when Stdlib.Int.equal lb ub ->
    Format.fprintf fmt "%i" lb
  | Some lb, Some ub ->
    Format.fprintf fmt "(%i..%i)" lb ub
  | None, Some ub ->
    Format.fprintf fmt "(..%i)" ub
  | Some lb, None ->
    Format.fprintf fmt "(%i..)" lb

