open Sstt
open Rstt_utils

(* Vector partitionning *)
let partition_vecs tc =
  if Tag.equal (TagComp.tag tc) Vec.tag then
    let ty = Descr.mk_tagcomp tc |> Ty.mk_descr in
    let ty = Vec.partition |> List.map (Ty.cap ty) |> Ty.disj in
    Ty.get_descr ty |> Descr.get_tags |> Tags.get Vec.tag
  else tc
let partition_vecs tags =
  let b,tags = Tags.destruct tags in
  let tags = tags |> List.map partition_vecs in
  Tags.construct (b,tags)
let partition_vecs d =
  Descr.set_component d (Descr.Tags (Descr.get_tags d |> partition_vecs))
let partition_vecs vd =
  VDescr.map partition_vecs vd
let partition_vecs ty =
  Transform.transform partition_vecs ty

(* Type simplification *)
let simplify_dnf to_ty dnf =
  let decorate_atom a =
    (to_ty a |> partition_vecs), a
  in
  let decorate_line' (ps,ns) =
    let pty, nty = List.map fst ps, List.map fst ns in
    let ty = Ty.cap (Ty.conj pty) (Ty.disj nty |> Ty.neg) in
    ty, (ps,ns)
  in
  let decorate_line (ps,ns) =
    let ps = List.map decorate_atom ps in
    let ns = List.map decorate_atom ns in
    decorate_line' (ps,ns)
  in
  let undecorate_line (_, (ps,ns)) =
    List.map snd ps, List.map snd ns
  in
  let dnf = dnf |> List.map decorate_line in
  let ty = List.map fst dnf |> Ty.disj in
  (* Remove useless clauses *)
  let dnf = dnf |> map_among_others (fun (_, (cp, cn)) c_others ->
      let ty_others = List.map fst c_others |> Ty.disj in
      let ty_p = List.map fst cp |> Ty.conj in
      let ty_n = List.map fst cn |> Ty.disj |> Ty.neg in
      let cp = cp |> filter_among_others (fun _ cp_others ->
          Ty.leq (Ty.cup (Ty.cap (List.map fst cp_others |> Ty.conj) ty_n) ty_others) ty |> not
        ) in
      let cn = cn |> filter_among_others (fun _ cn_others ->
          Ty.leq (Ty.cup (Ty.cap (List.map fst cn_others |> Ty.disj |> Ty.neg) ty_p) ty_others) ty |> not
        ) in
      decorate_line' (cp, cn)
    )
  in
  (* Remove useless summands (must be done AFTER clauses simplification) *)
  let dnf = dnf |> filter_among_others (fun (ty_c,_) c_others ->
      let ty_others = List.map fst c_others |> Ty.disj in
      Ty.leq (Ty.cup ty_c ty_others) ty_others |> not
    ) in
  List.map undecorate_line dnf

let simpl_arrows arrows =
  let to_ty a = Descr.mk_arrow a |> Ty.mk_descr in
  Arrows.dnf arrows |> simplify_dnf to_ty |> Arrows.of_dnf
let simpl_records records =
  let to_ty a = Descr.mk_record a |> Ty.mk_descr in
  Records.dnf records |> simplify_dnf to_ty |> Records.of_dnf
let simpl_tuples p =
  let n = TupleComp.len p in
  let to_ty a = Descr.mk_tuple a |> Ty.mk_descr in
  TupleComp.dnf p |> simplify_dnf to_ty |> TupleComp.of_dnf n
let simpl_tuples t =
  let b, comps = Tuples.destruct t in
  let comps = List.map simpl_tuples comps in
  Tuples.construct (b, comps)
let simpl_tags c =
  let tag = TagComp.tag c in
  let to_ty a = Descr.mk_tag a |> Ty.mk_descr in
  TagComp.dnf c |> simplify_dnf to_ty |> TagComp.of_dnf tag
let simpl_tags t =
    let b, comps = Tags.destruct t in
    let comps = List.map simpl_tags comps in
    Tags.construct (b,comps)

let simpl_descr d =
  let open Descr in
  d |> components |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (simpl_tags t)
      | Arrows a -> Arrows (simpl_arrows a)
      | Tuples t -> Tuples (simpl_tuples t)
      | Records r -> Records (simpl_records r)
    ) |>  Descr.of_components

let simpl_vdescr = VDescr.map simpl_descr
let normalize = Transform.transform simpl_vdescr

let leq_partition ty1 ty2 = Ty.diff ty1 ty2 |> normalize |> Ty.is_empty
let simplify ty =
  normalize ty |> Transform.simplify
