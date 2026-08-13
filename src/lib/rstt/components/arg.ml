open Sstt
open Rstt_utils

let tag = Tag.mk "arg"
module Reserved = Labels.Reserved

let add_tag ty = TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
let proj_tag ty =
  ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> Op.TagComp.as_atom |> snd
let npos_field n = Reserved.npos, Intervals.Atom.mk_singl n |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr
let npos_field' n = Reserved.npos, Intervals.Atom.mk (Some n) None |> Descr.mk_interval |> Ty.mk_descr
  |> Ty.O.required |> Ty.F.mk_descr

type ('l,'f) atom = { pos_named : ('l * 'f) list ; pos_tl: 'f ; named_tl : 'f ; named : ('l * 'f) list }
type ('l,'f) atom' = { pos' : 'f list ; pos_tl': 'f ; named' : ('l * 'f) list ; named_tl' : 'f }
type ('l,'f) elt =
| DefSite of ('l,'f) atom
| CallSite of  ('l,'f) atom'
type ('l,'f) t = ('l,'f) elt list
let map_atom fl f { pos_named ; pos_tl ; named_tl ; named } =
  let aux (lbl,t) = fl lbl, f t in
  let pos_named = List.map aux pos_named in
  let named = List.map aux named in
  let pos_tl, named_tl = f pos_tl, f named_tl in
  { pos_named ; pos_tl ; named_tl ; named }
let map_atom' fl f { pos' ; pos_tl' ; named' ; named_tl' } =
  let pos' = List.map f pos' in
  let named' = List.map (fun (lbl,t) -> fl lbl, f t) named' in
  let pos_tl', named_tl' = f pos_tl', f named_tl' in
  { pos' ; pos_tl' ; named' ; named_tl' }
let map_elt f t =
  match t with
  | DefSite a -> DefSite (map_atom Fun.id f a)
  | CallSite a -> CallSite (map_atom' Fun.id f a)
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
let mk' ?allow_more_pos ~id { pos' ; pos_tl' ; named' ; named_tl' } =
  let record t = t |> Descr.mk_record |> Ty.mk_descr |> Ty.O.required |> Ty.F.mk_descr in
  let pos_tl' = Utils.constant_oty_part pos_tl' |> Ty.O.get in
  let id = (match id with None -> Ty.empty | Some id -> Descr.mk_enum id |> Ty.mk_descr)
  |> Ty.O.optional |> Ty.F.mk_descr in
  let allow_more_pos =
    match allow_more_pos with
    | Some b -> b
    | None -> pos_tl' |> Ty.O.Atom.get |> Ty.is_empty |> not
  in
  let pos_tl' = if allow_more_pos then Ty.O.mk pos_tl' else Ty.O.absent in
  let pos_bindings = pos' |> List.mapi (fun i fty -> Labels.pos i, fty) |> LabelMap.of_list in
  let pos = Reserved.pos, { Records.Atom.bindings=pos_bindings ;
    tail=Ty.F.mk_descr pos_tl' } |> record in
  let named_bindings = named' |> List.map (fun (str, fty) -> Labels.named str, fty) |> LabelMap.of_list in
  let named = Reserved.named,
    { Records.Atom.bindings=named_bindings ; tail=Utils.add_option named_tl' } |> record in
  let n = List.length pos' |> Z.of_int in
  let npos = if allow_more_pos then npos_field' n else npos_field n in
  let bindings = [Reserved.id,id ; npos ; pos ; named] |> LabelMap.of_list in
  { Records.Atom.bindings ; tail=Ty.F.any } |> Descr.mk_record |> Ty.mk_descr |> add_tag
let mk { pos_named ; pos_tl ; named_tl ; named } =
  let id = fresh_id () in
  let fsig = map_atom Fun.id (Fun.const ()) { pos_named ; pos_tl ; named_tl ; named } in
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
let mk' = mk' ?allow_more_pos:None ~id:None
let any_id = Enums.any |> Descr.mk_enums |> Ty.mk_descr
|> Ty.O.optional |> Ty.F.mk_descr
let any_d =
  { Records.Atom.bindings=[
      Reserved.id, any_id ;
      npos_field' (Z.minus_one)] |> LabelMap.of_list ;
    tail=Ty.F.any }
  |> Descr.mk_record |> Ty.mk_descr
let any = add_tag any_d

let extract_ids (a:Records.Atom'.t) =
  let enums = Records.Atom'.find Reserved.id a |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get
  |> Ty.get_descr |> Descr.get_enums in
  match Enums.destruct enums with
  | true, lst -> Some lst
  | false, _ -> None

let params_of_id id = Hashtbl.find sigs id
let extract ty : (string, Ty.F.t) t =
  if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid arg encoding." ;
  let extract_record lbl a = Records.Atom'.find lbl a |> Ty.F.get_descr
    |> Ty.O.get |> Ty.O.Atom.get |> Ty.get_descr |> Descr.get_records |> Op.Records'.approx in
  let extract_npos_min a = Records.Atom'.find Reserved.npos a |> Ty.F.get_descr
    |> Ty.O.get |> Ty.O.Atom.get |> Ty.get_descr |> Descr.get_intervals |> Intervals.lb
    |> Option.get |> Z.to_int in
  let extract_defsite id a =
    let fsig = Hashtbl.find sigs id in
    if List.length fsig.pos_named <> extract_npos_min a then None
    else
      let apos, anamed = extract_record Reserved.pos a, extract_record Reserved.named a in
      let pos_named = fsig.pos_named |> List.mapi (fun i (name,()) ->
        name, Op.Records'.Atom.find (Labels.pos i) apos)
      in
      let named = fsig.named |> List.map (fun (name,()) ->
        name, Op.Records'.Atom.find (Labels.named name) anamed)
      in
      let pos_tl, named_tl = apos.Op.Records'.Atom.tail, anamed.Op.Records'.Atom.tail in
      Some { pos_named ; pos_tl ; named_tl ; named }
  in
  let extract_callsite a =
    let npos, apos, anamed = extract_npos_min a,
      extract_record Reserved.pos a, extract_record Reserved.named a in
    let pos' = List.init npos Fun.id |> List.map (fun i ->
        Op.Records'.Atom.find (Labels.pos i) apos)
    in      
    let named' = anamed.Op.Records'.Atom.bindings |> Op.Records'.Atom.LabelMap.bindings |>
      List.map (fun (lbl,ty) ->
        match Labels.info lbl with
        | Labels.Named str -> (str,ty)
        | Labels.Pos _ -> assert false
        | exception Invalid_argument _ -> assert false
        ) in
    let pos_tl', named_tl' = apos.Op.Records'.Atom.tail, anamed.Op.Records'.Atom.tail in
    { pos' ; pos_tl' ; named' ; named_tl' }
  in
  let extract a =
    match extract_ids a with
    | Some (id::_) ->
      extract_defsite id a |> Option.map (fun x -> DefSite x)
    | Some [] -> Some (CallSite (extract_callsite a))
    | None -> (* Any *) Some
      (DefSite {pos_named=[];pos_tl=Ty.F.any;named=[];named_tl=Ty.F.any})
  in
  let lines = Ty.get_descr ty |> Descr.get_records |> Records.dnf' in
  List.filter_map extract lines
let to_t ctx comp =
  let ty = Op.TagComp.as_atom comp |> snd in
  if Ty.leq ty any_d
  then Some (extract ty |> map ctx.Printer.build_field)
  else None

let destruct ty =
  proj_tag ty |> Ty.cap any_d |> extract

let reidentify ~id ty =
  let id = id |> Ty.O.optional |> Ty.F.mk_descr |> Ty.F.cap any_id in
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
  let cmp t1 t2 =
    let open Rstt_utils in
    let cmp_pos_bindings = List.compare Pp.Compare.fdescr in
    let cmp_field (str1,f1) (str2,f2) =
      String.compare str1 str2 |> ccmp Pp.Compare.fdescr f1 f2
    in
    let cmp_bindings b1 b2 = List.compare cmp_field b1 b2 in
    let cmp_def a1 a2 =
      Pp.Compare.fdescr a1.pos_tl a2.pos_tl |> ccmp
      Pp.Compare.fdescr a1.named_tl a2.named_tl |> ccmp
      cmp_bindings a1.pos_named a2.pos_named |> ccmp
      cmp_bindings a1.named a2.named
    in
    let cmp_call a1 a2 =
      Pp.Compare.fdescr a1.pos_tl' a2.pos_tl' |> ccmp
      Pp.Compare.fdescr a1.named_tl' a2.named_tl' |> ccmp
      cmp_pos_bindings a1.pos' a2.pos' |> ccmp
      cmp_bindings a1.named' a2.named'
    in
    match t1, t2 with
    | DefSite a1, DefSite a2 -> cmp_def a1 a2
    | CallSite a1, CallSite a2 -> cmp_call a1 a2
    | DefSite _, _ -> -1 | _, DefSite _ -> 1
  in
  let is_absent fd = Ty.F.equiv fd.Printer.fty (Ty.F.mk_descr Ty.O.absent) in
  let print_field_ty fmt f =
    match f with
    | f when is_absent f -> Format.fprintf fmt "absent"
    | f -> Printer.print_field_ctx Prec.min_prec Prec.NoAssoc fmt f
  in
  let print_field fmt (name,ty) = Format.fprintf fmt "%s: %a" name print_field_ty ty in
  let print_tail fmt (f',f) =
    match f', f with
    | { Printer.fty=fty1 ; _ }, { Printer.fty=fty2 ; _ }
      when Ty.F.equiv fty1 (Utils.constant_oty_part fty2 |> Ty.F.mk_descr) ->
      Format.fprintf fmt "...: %a" print_field_ty (Utils.prune_option_fdescr f)
    | f', f -> Format.fprintf fmt "...: (%a, %a)"
      print_field_ty (Utils.prune_option_fdescr f')
      print_field_ty (Utils.prune_option_fdescr f)
  in
  let print_elt fmt e =
    match e with
    | `PosField ty -> print_field_ty fmt ty
    | `Field f -> print_field fmt f
    | `Tail t -> print_tail fmt t
    | `NamedField f -> print_field fmt f
  in
  let print_atom _prec _assoc fmt a =
    let pos_named, tl, named =
      List.map (fun (str,t) -> `Field (str, t)) a.pos_named,
      `Tail (a.pos_tl, a.named_tl),
      List.map (fun (str,t) -> `NamedField (str, t)) a.named in
    let seq =
      if List.is_empty named && is_absent a.pos_tl && is_absent a.named_tl
      then pos_named
      else pos_named@[tl]@named
    in
    Format.fprintf fmt "(%a)" (print_seq print_elt ", ") seq
  in
  let print_atom' _prec _assoc fmt a =
    let pos, named, tl =
      List.map (fun t -> `PosField t) a.pos',
      List.map (fun (str,t) -> `NamedField (str,t)) a.named',
      `Tail (a.pos_tl', a.named_tl') in
    let seq =
      if is_absent a.pos_tl' && is_absent a.named_tl'
      then pos@named
      else pos@named@[tl]
    in
    Format.fprintf fmt "@(%a)" (print_seq print_elt ", ") seq
  in
  let print_any_atom prec assoc fmt elt =
    match elt with
    | DefSite a -> print_atom prec assoc fmt a
    | CallSite a -> print_atom' prec assoc fmt a
  in
  Pp.print_cup ~cmp print_any_atom prec assoc fmt t
(* let print = Utils.struct_print print *) (* Args are not packed in Attr *)

let printer_builder =
  Printer.builder ~to_t:to_t ~map:(fun _ ff -> map ff) ~print:print
let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
let () = Pp.add_printer_param printer_params
