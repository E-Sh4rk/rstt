open Sstt

(* ===== SIMPLIFY ===== *)

let simpl_tags c =
  let tag = TagComp.tag c in
  if Tag.equal tag Attr.tag
  then
    let ty = Tags.mk_comp c |> Descr.mk_tags |> Ty.mk_descr in
    let anyattr, others = Attr.destruct ty |> List.partition_map (fun (ps,ns) ->
      let content = List.map (fun x -> x.Attr.content) ps |> Ty.conj in
      let classes = List.map (fun x -> x.Attr.classes) ps |> Ty.conj in
      let attrs = List.map (fun x -> x.Attr.attrs) ps |> Ty.conj in
      let p = {Attr.content;classes;attrs} in
      if List.is_empty ns && Ty.equiv (Attr.mk p) (Attr.mk_content content)
      then Either.left content
      else Either.right (Attr.mk_line ([p],ns))
      ) in
    let anyattr = Ty.disj anyattr |> Attr.mk_content in
    Ty.disj (anyattr::others) |> Ty.get_descr |> Descr.get_tags |> Tags.get Attr.tag
  else if Tag.equal tag Vec.tag
  then
    let ty = Tags.mk_comp c |> Descr.mk_tags |> Ty.mk_descr in
    let solo, others = Vec.destruct ty |> List.partition_map (fun (p,ns) ->
      if List.is_empty ns
      then Either.left p
      else Either.right (Vec.mk_line (p,ns))
      ) in
    let solo_vec, solo_scal = solo |> List.partition_map
      (function Vec.Vector c -> Either.left c | Vec.Scalar c -> Either.right c) in
    let solo_vec = Vec.Vector (Ty.disj solo_vec) |> Vec.mk in
    let solo_scal = Vec.Scalar (Ty.disj solo_scal) |> Vec.mk in
    Ty.disj (solo_vec::solo_scal::others) |> Ty.get_descr |> Descr.get_tags |> Tags.get Vec.tag
  else
    c
let simpl_tags t =
    let b, comps = Tags.destruct t in
    let comps = List.map simpl_tags comps in
    Tags.construct (b,comps)
let simpl_descr d =
  let open Descr in
  let b, comps = destruct d in
  let comps = comps |> List.map (function
      | Intervals i -> Intervals i
      | Enums e -> Enums e
      | Tags t -> Tags (simpl_tags t)
      | Arrows a -> Arrows a
      | Tuples t -> Tuples t
      | Records r -> Records r
    ) in
    construct (b, comps)
let extra vd = VDescr.map simpl_descr vd
let simplify t = Transform.simplify ~extra t

(* ===== TALLY ===== *)

let normalize_subst s =
  (* TODO *)
  Some s

let tally delta cs =
  Tallying.tally delta cs
  |> List.filter_map normalize_subst
