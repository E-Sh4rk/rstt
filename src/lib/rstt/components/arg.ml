open Sstt
open Rstt_utils

let tag = Tag.mk "arg"
let id_label = Label.mk "_id"
let dummy_id = Enum.mk "dummy"
let npos_label = Label.mk "_npos"

let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd
let npos_field n = npos_label, Intervals.Atom.mk_singl n |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr
let npos_field' n = npos_label, Intervals.Atom.mk (Some n) None |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr

type 'a atom = { pos : 'a list ; pos_named : (string * 'a) list ; tl : 'a ; named : (string * 'a) list }
type 'a atom' = { pos' : 'a list ; tl' : 'a ; named' : (string * 'a) list }
type 'a elt =
| DefSite of 'a atom
| CallSite of 'a atom'
type 'a t = 'a elt list
(* type fsig = unit atom *)
let map_atom f { pos ; pos_named ; tl ; named } =
  let pos = List.map f pos in
  let pos_named = List.map (fun (str,t) -> str, f t) pos_named in
  let named = List.map (fun (str,t) -> str, f t) named in
  let tl = f tl in
  { pos ; pos_named ; named ; tl }
let map_atom' f { pos' ; tl' ; named' } =
  let pos' = List.map f pos' in
  let named' = List.map (fun (str,t) -> str, f t) named' in
  let tl' = f tl' in
  { pos' ; named' ; tl' }
let map_elt f t =
  match t with
  | DefSite a -> DefSite (map_atom f a)
  | CallSite a -> CallSite (map_atom' f a)
let map f t = List.map (map_elt f) t

let sigs = Hashtbl.create 100

let split_at_index lst n =
  let rec aux acc next n =
    if n = 0 then List.rev acc, next
    else match next with
    | [] -> assert false
    | p::defs -> aux (p::acc) defs (n-1)
  in
  aux [] lst n

let fresh_id =
  let i = ref 0 in
  fun () ->
    i := !i+1 ;
    Enum.mk (string_of_int !i)
let mk' ~allow_more_pos ~id { pos' ; tl' ; named' } =
  let id = match id with
  | None -> [dummy_id]
  | Some id -> [dummy_id ; id]
  in
  let id = id |> List.map Descr.mk_enum |> List.map Ty.mk_descr
  |> Ty.disj |> Ty.O.required |> Ty.F.mk_descr in
  let more_pos = allow_more_pos && (Ty.F.get_descr tl' |> Ty.O.get |> Ty.is_empty |> not) in
  let n = List.length pos' |> Z.of_int in
  let pos = pos' |> List.mapi (fun i fty -> Labels.pos i, fty) in
  let named = named' |> List.map (fun (str, fty) -> Labels.named str, fty) in
  let npos = if more_pos then npos_field' n else npos_field n in
  let bindings = (id_label, id)::npos::pos@named |> LabelMap.of_list in
  let tail = Ty.F.cup tl' (Ty.F.mk_descr Ty.O.absent) in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk { pos ; pos_named ; tl ; named } =
  let id = fresh_id () in
  let fsig = map_atom (fun _ -> ()) { pos ; pos_named ; tl ; named } in
  Hashtbl.add sigs id fsig ;
  let n = List.length pos_named in
  let atoms' = List.init (n + 1) (fun i ->
    let pos', named' = split_at_index pos_named i in
    let pos' = pos@(pos' |> List.map (fun (_,fty) -> fty)) in
    let named' = named@named' in
    mk' ~allow_more_pos:(i=n) ~id:(Some id) { pos' ; named' ; tl'=tl }
  ) in
  atoms' |> Ty.disj
let mk' = mk' ~allow_more_pos:true ~id:None
let any_d =
  { Records.Atom.bindings=[
      id_label, Enums.any |> Descr.mk_enums |> Ty.mk_descr |> Ty.O.required |> Ty.F.mk_descr ;
      npos_field' (Z.minus_one)] |> LabelMap.of_list ;
    tail=Ty.F.any }
  |> Descr.mk_record |> Ty.mk_descr
let any = add_tag any_d

let extract_id (a:Records.Atom'.t) =
  let enums = Records.Atom'.find id_label a |> Ty.F.get_descr |> Ty.O.get
  |> Ty.get_descr |> Descr.get_enums in
  match Enums.destruct enums with
  | true, lst ->
    begin match List.filter (fun e -> Enum.equal dummy_id e |> not) lst with
    | [id] -> Some id
    | _ -> None
    end
  | _, _ -> None

let extract ty : Ty.F.t t =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid arg encoding." ;
  let extract_defsite id a =
    let fsig = Hashtbl.find sigs id in
    let pos = fsig.pos |> List.mapi (fun i () ->
      let lbl = Labels.pos i in
      Records.Atom'.find lbl a
      ) in
    let pos_named = fsig.pos_named |> List.map (fun (name,()) ->
      let lbl = Labels.named name in
      name, Records.Atom'.find lbl a
    )
    in
    let tl = a.Records.Atom'.tail in
    let named = fsig.named |> List.map (fun (name,()) ->
      let lbl = Labels.named name in
      name, Records.Atom'.find lbl a
    )
    in
    { pos ; pos_named ; tl ; named }
  in
  let extract_callsite a =
    let bindings = a.Records.Atom'.bindings |> LabelMap.bindings |>
      List.filter_map (fun (lbl,ty) -> try Some (Labels.info lbl, ty) with Invalid_argument _ -> None) in
    let pos' = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.Pos i -> Some (i,ty) | _ -> None) in
    let named' = bindings |> List.filter_map (fun (i,ty) -> match i with Labels.Named str -> Some (str,ty) | _ -> None) in
    let pos' = List.sort (fun (i1,_) (i2,_) -> Stdlib.Int.compare i1 i2) pos' |> List.map snd in
    { pos' ; tl'=a.Records.Atom'.tail ; named' }
  in
  let npos_zero = Intervals.Atom.mk_singl Z.zero |> Descr.mk_interval |> Ty.mk_descr in
  let extract a =
    match extract_id a with
    | Some id ->
      if Ty.cap (Records.Atom'.find npos_label a |> Ty.F.get_descr |> Ty.O.get) npos_zero
        |> Ty.is_empty then None
      else Some (DefSite (extract_defsite id a))
    | None -> Some (CallSite (extract_callsite a))
  in
  let lines = Ty.get_descr ty |> Descr.get_records |> Records.dnf' in
  List.filter_map extract lines
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d then Some (extract ty |> map ctx.Printer.build_fop)
  else None

let destruct ty =
  proj_tag ty |> extract

let print prec assoc fmt t =
  let print_field_ty = Printer.print_field_ctx Prec.min_prec Prec.NoAssoc in
  let print_field fmt (name,ty) =
      match name with
      | None -> Format.fprintf fmt "%a" print_field_ty ty
      | Some str -> Format.fprintf fmt "%s: %a" str print_field_ty ty
  in
  let print_tail fmt f =
    match f with
    | Printer.FTy (t, true) when Ty.leq t.Printer.ty Ty.empty -> ()
    | Printer.FTy (t, true) when Ty.leq Attr.any t.ty -> Format.fprintf fmt "... "
    | f -> Format.fprintf fmt "; %a " print_field_ty (Utils.prune_option_fop f)
  in
  let print_atom _prec _assoc fmt a =
    let pos, named, pos_named =
      List.map (fun t -> None, t) a.pos,
      List.map (fun (str,t) -> Some str, t) a.named,
      List.map (fun (str,t) -> Some str, t) a.pos_named in
    Format.fprintf fmt "( %a %a%s%a)" (print_seq print_field ", ")
      (pos@pos_named) print_tail a.tl (if named = [] then "" else "; ")
      (print_seq print_field ", ") named
  in
  let print_atom' _prec _assoc fmt a =
    let pos, named =
      List.map (fun t -> None, t) a.pos',
      List.map (fun (str,t) -> Some str, t) a.named' in
    Format.fprintf fmt "@( %a %a)" (print_seq print_field ", ")
      (pos@named) print_tail a.tl'
  in
  let print_elt prec assoc fmt elt =
    match elt with
    | DefSite a -> print_atom prec assoc fmt a
    | CallSite a -> print_atom' prec assoc fmt a
  in
  Prec.print_cup print_elt prec assoc fmt t

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun f -> map (Printer.map_fop f)) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
