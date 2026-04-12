open Sstt
open Rstt_utils

let tag = Tag.mk "arg"
let dummy_id = Enum.mk "dummy"
module Reserved = Labels.Reserved

let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd
let npos_field n = Reserved.npos, Intervals.Atom.mk_singl n |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr
let npos_field' n = Reserved.npos, Intervals.Atom.mk (Some n) None |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr

type ('f, 't) atom = { pos_named : (string * 'f) list ; pos_tl: 't ; named_tl : 'f ; named : (string * 'f) list }
type ('f, 't) atom' = { pos' : 'f list ; pos_tl': 't ; named' : (string * 'f) list ; named_tl' : 'f }
type ('f, 't) elt =
| DefSite of ('f, 't) atom
| CallSite of  ('f, 't) atom'
type ('f, 't) t = ('f, 't) elt list
(* type fsig = unit atom *)
let map_atom ff fo { pos_named ; pos_tl ; named_tl ; named } =
  let pos_named = List.map (fun (str,t) -> str, ff t) pos_named in
  let named = List.map (fun (str,t) -> str, ff t) named in
  let pos_tl, named_tl = fo pos_tl, ff named_tl in
  { pos_named ; pos_tl ; named_tl ; named }
let map_atom' ff fo { pos' ; pos_tl' ; named' ; named_tl' } =
  let pos' = List.map ff pos' in
  let named' = List.map (fun (str,t) -> str, ff t) named' in
  let pos_tl', named_tl' = fo pos_tl', ff named_tl' in
  { pos' ; pos_tl' ; named' ; named_tl' }
let map_elt ff fo t =
  match t with
  | DefSite a -> DefSite (map_atom ff fo a)
  | CallSite a -> CallSite (map_atom' ff fo a)
let map ff fo t = List.map (map_elt ff fo) t

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
let mk' ~allow_more_pos ~id { pos' ; pos_tl' ; named' ; named_tl' } =
  let id = match id with
  | None -> [dummy_id]
  | Some id -> [dummy_id ; id]
  in
  let id = id |> List.map Descr.mk_enum |> List.map Ty.mk_descr
  |> Ty.disj |> Ty.O.required |> Ty.F.mk_descr in
  let allow_more_pos = allow_more_pos && (pos_tl' |> Ty.is_empty |> not) in
  let pos_tl' = if allow_more_pos then pos_tl' else Ty.empty in
  let pos_bindings = pos' |> List.mapi (fun i fty -> Labels.pos i, fty) |> LabelMap.of_list in
  let pos = Reserved.pos, { Records.Atom.bindings=pos_bindings ;
    tail=Ty.O.optional pos_tl' |> Ty.F.mk_descr }
  |> Descr.mk_record |> Ty.mk_descr |> Ty.O.required |> Ty.F.mk_descr in
  let named = named' |> List.map (fun (str, fty) -> Labels.named str, fty) in
  let n = List.length pos' |> Z.of_int in
  let npos = if allow_more_pos then npos_field' n else npos_field n in
  let bindings = (Reserved.id, id)::npos::pos::named |> LabelMap.of_list in
  let tail = Utils.add_option named_tl' in
  { Records.Atom.bindings ; tail } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk { pos_named ; pos_tl ; named_tl ; named } =
  let id = fresh_id () in
  let fsig = map_atom (Fun.const ()) (Fun.const ()) { pos_named ; pos_tl ; named_tl ; named } in
  Hashtbl.add sigs id fsig ;
  let n = List.length pos_named in
  (* let k = List.length pos in *)
  let atoms' = List.init (n + 1) (fun i ->
    let pos', named' = split_at_index pos_named i in
    let pos' = pos' |> List.map (fun (_,fty) -> fty) in
    let named' = named'@named in
    mk' ~allow_more_pos:(i=n) ~id:(Some id)
      { pos' ; named' ; pos_tl'=pos_tl ; named_tl'=named_tl }
  ) in
  atoms' |> Ty.disj
let mk' = mk' ~allow_more_pos:true ~id:None
let any_id = Enums.any |> Descr.mk_enums |> Ty.mk_descr |> Ty.O.required |> Ty.F.mk_descr
let any_d =
  { Records.Atom.bindings=[
      Reserved.id, any_id ;
      npos_field' (Z.minus_one)] |> LabelMap.of_list ;
    tail=Ty.F.any }
  |> Descr.mk_record |> Ty.mk_descr
let any = add_tag any_d

let extract_ids (a:Records.Atom'.t) =
  let enums = Records.Atom'.find Reserved.id a |> Ty.F.get_descr |> Ty.O.get
  |> Ty.get_descr |> Descr.get_enums in
  match Enums.destruct enums with
  | true, lst -> Some (List.filter (fun e -> Enum.equal dummy_id e |> not) lst)
  | false, _ -> None

(* TODO: push named in a its own field just like pos (this avoids having reserved fields in Lst) *)
let params_of_id id = Hashtbl.find sigs id
let extract ty : (Ty.F.t, Ty.t) t =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid arg encoding." ;
  let extract_pos a = Records.Atom'.find Reserved.pos a |> Ty.F.get_descr
    |> Ty.O.get |> Ty.get_descr |> Descr.get_records |> Op.Records'.approx in
  let extract_npos_min a = Records.Atom'.find Reserved.npos a |> Ty.F.get_descr
    |> Ty.O.get |> Ty.get_descr |> Descr.get_intervals |> Intervals.lb
    |> Option.get |> Z.to_int in
  let extract_defsite id a =
    let fsig = Hashtbl.find sigs id in
    if List.length fsig.pos_named <> extract_npos_min a then None
    else
      let apos = extract_pos a in
      let pos_named = fsig.pos_named |> List.mapi (fun i (name,()) ->
        name, Op.Records'.Atom.find (Labels.pos i) apos)
      in
      let pos_tl = apos.Op.Records'.Atom.tail |> Ty.F.get_descr |> Ty.O.get in
      let named_tl = a.Records.Atom'.tail in
      let named = fsig.named |> List.map (fun (name,()) ->
        let lbl = Labels.named name in
        name, Records.Atom'.find lbl a
      )
      in
      Some { pos_named ; pos_tl ; named_tl ; named }
  in
  let extract_callsite a =
    let npos, apos = extract_npos_min a, extract_pos a in
    let pos' = List.init npos Fun.id |> List.map (fun i ->
        Op.Records'.Atom.find (Labels.pos i) apos)
    in      
    let named' = a.Records.Atom'.bindings |> LabelMap.bindings |>
      List.filter_map (fun (lbl,ty) ->
        match Labels.info lbl with
        | Labels.Named str -> Some (str,ty)
        | Labels.Pos _ | Labels.Sym _ -> None
        | exception Invalid_argument _ -> None
        ) in
    let pos_tl' = apos.Op.Records'.Atom.tail |> Ty.F.get_descr |> Ty.O.get in
    let named_tl' = a.Records.Atom'.tail in
    { pos' ; pos_tl' ; named' ; named_tl' }
  in
  let extract a =
    match extract_ids a with
    | Some (id::_) ->
      extract_defsite id a |> Option.map (fun x -> DefSite x)
    | Some [] -> Some (CallSite (extract_callsite a))
    | None -> (* Any *) Some
      (DefSite {pos_named=[];pos_tl=Ty.any;named=[];named_tl=Ty.F.any})
  in
  let lines = Ty.get_descr ty |> Descr.get_records |> Records.dnf' in
  List.filter_map extract lines
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d
  then Some (extract ty |> map ctx.Printer.build_fop ctx.Printer.build)
  else None

let destruct ty =
  proj_tag ty |> Ty.cap any_d |> extract

let reidentify ~id ty =
  let id = Ty.cup id (Descr.mk_enum dummy_id |> Ty.mk_descr)
  |> Ty.O.required |> Ty.F.mk_descr |> Ty.F.cap any_id in
  let aux { Records.Atom.bindings ; tail } =
    let bindings = LabelMap.add Reserved.id id bindings in
    { Records.Atom.bindings ; tail }
  in
  let ty = proj_tag ty in
  let ty = Ty.get_descr ty |> Descr.get_records |> Records.dnf
    |> List.map (fun (ps, _) -> (List.map aux ps, []))
    |> Records.of_dnf |> Descr.mk_records |> Ty.mk_descr
  in
  add_tag ty

let ids_of ty =
  proj_tag ty |> Ty.get_descr |> Descr.get_records |> Records.dnf'
  |> List.map extract_ids |> List.filter_map Fun.id |> List.concat

let print prec assoc fmt t =
  let print_field_ty = Printer.print_field_ctx Prec.min_prec Prec.NoAssoc in
  let print_ty = Printer.print_descr_ctx Prec.min_prec Prec.NoAssoc in
  let print_field fmt (name,ty) =
      match name with
      | None -> Format.fprintf fmt "%a" print_field_ty ty
      | Some str -> Format.fprintf fmt "%s: %a" str print_field_ty ty
  in
  let print_tail fmt (ty,f) =
    match ty, f with
    | t', Printer.FTy (t, true) when Ty.is_empty t'.Printer.ty && Ty.is_empty t.Printer.ty -> ()
    | t', Printer.FTy (t, true) when Ty.equiv t.Printer.ty t'.Printer.ty ->
      Format.fprintf fmt "; %a " print_ty t'
    | t', f -> Format.fprintf fmt "; %a, %a " print_ty t' print_field_ty (Utils.prune_option_fop f)
  in
  let print_atom _prec _assoc fmt a =
    let named, pos_named =
      List.map (fun (str,t) -> Some str, t) a.named,
      List.map (fun (str,t) -> Some str, t) a.pos_named in
    Format.fprintf fmt "( %a %a%s%a)" (print_seq print_field ", ")
      (pos_named) print_tail (a.pos_tl, a.named_tl) (if named = [] then "" else "; ")
      (print_seq print_field ", ") named
  in
  let print_atom' _prec _assoc fmt a =
    let pos, named =
      List.map (fun t -> None, t) a.pos',
      List.map (fun (str,t) -> Some str, t) a.named' in
    Format.fprintf fmt "@( %a %a)" (print_seq print_field ", ")
      (pos@named) print_tail (a.pos_tl', a.named_tl')
  in
  let print_elt prec assoc fmt elt =
    match elt with
    | DefSite a -> print_atom prec assoc fmt a
    | CallSite a -> print_atom' prec assoc fmt a
  in
  Prec.print_cup print_elt prec assoc fmt t

let printer_builder =
  let map f =
    let f' x = (f x).Printer.op in
    map (Printer.map_fop f) (Printer.map_descr f')
  in
  Printer.builder ~to_t:to_t ~map ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
