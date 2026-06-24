open Sstt

let rec partition_map3 f lst =
  match lst with
  | [] -> [],[],[]
  | e::lst ->
    let l1,l2,l3 = partition_map3 f lst in
    begin match f e with
    | `A a -> a::l1,l2,l3
    | `B b -> l1,b::l2,l3
    | `C c -> l1,l2,c::l3
    end

let map_tags f d =
  let open Descr in
  let comps, b = components d in
  let comps = comps |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (f t)
      | Arrows a -> Arrows a
      | Tuples t -> Tuples t
      | Records r -> Records r
    ) in
  of_components (comps, b)
let map_tag_content f tag d =
  let f tc =
    if Tag.equal (TagComp.tag tc) tag
    then TagComp.mk (tag, f (Op.TagComp.as_atom tc |> snd))
    else tc
  in
  let f t = Tags.map f t in
  map_tags f d

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

let prune_option_fdescr fd =
  let rec aux fd =
    let fop = match fd.Printer.fop with
    | FTy (ty,_) when ty.Printer.ty |> Ty.is_empty |> not -> Printer.FTy (ty,false)
    | FTy (ty,o) -> Printer.FTy (ty,o)
    | FVarop (o,es) -> FVarop (o, List.map aux es)
    | FBinop (o,e1,e2) -> FBinop (o, aux e1, aux e2)
    | FUnop (o,e) -> FUnop (o, aux e)
    | FRowVar v -> FRowVar v
    in
    { fd with fop }
  in
  aux fd

let add_option tyo =
  let ty = Ty.O.get tyo |> Ty.O.Atom.get in
  if Ty.is_empty ty then tyo else ty |> Ty.O.optional
let add_option fty =
  fty |> Ty.F.map add_option

let constant_oty_part fty =
  Ty.F.dnf fty |> List.filter_map (fun (ps,ns,oty) ->
    if List.is_empty ps && List.is_empty ns then Some oty else None)
  |> Ty.O.disj |> Ty.O.get |> Ty.O.Atom.get |> Ty.O.optional

type interval = int option * int option
let print_interval any f _prec _assoc fmt (lb,ub) =
  match lb, ub with
  | None, None -> Format.fprintf fmt "%s" any
  | Some lb, Some ub when Stdlib.Int.equal lb ub ->
    Format.fprintf fmt "%a" f lb
  | Some lb, Some ub ->
    Format.fprintf fmt "(%a..%a)" f lb f ub
  | None, Some ub ->
    Format.fprintf fmt "(..%a)" f ub
  | Some lb, None ->
    Format.fprintf fmt "(%a..)" f lb

let struct_print f prec assoc fmt t =
  if Pp.current_pos () = Pp.Struct
  then f prec assoc fmt t
  else Pp.pp_struct_tag f prec assoc fmt t

type 'a atomic_line = { pos:bool ; prim:'a list ; pvs:Var.t list ; nvs:Var.t list }
type 'a atomic_t = 'a atomic_line list
type 'a atom = P of (bool * 'a list) | V of Var.t
let any_atomic_t = [{ pos=false ; prim=[] ; pvs=[] ; nvs=[] }]
let is_singleton f t =
  match t with
  | [{pos=true;prim=[p];pvs=[];nvs=[]}] -> f p
  | _ -> false
let may_not_feature_any f t =
  t |> List.exists (function
    | {pos=true;prim;_} -> List.for_all f prim
    | {pos=false;_} -> false
  )

let line_to_atoms { pos ; prim ; pvs ; nvs } =
  let hd = if not pos && prim = [] && pvs <> [] then [] else [P (pos, prim)] in
  hd@(List.map (fun x -> V x) pvs), (List.map (fun x -> V x) nvs)
let t_to_dnf x = List.map line_to_atoms x
