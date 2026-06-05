open Sstt

let transform_if_vectors f ty =
  try
    let _ = ty |> Ty.nodes |> List.map (fun ty ->
      Ty.def ty |> VDescr.map (fun d ->
        let (tags,_) = Descr.get_tags d |> Tags.components in
        List.map (fun tc ->
          if TagComp.tag tc |> Tag.equal Vec.tag
          then raise Exit ; tc) tags |> ignore ;
        d
      )) in
    ty
  with Exit -> Transform.transform f ty

(* Partition *)

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
let partition_vecs = transform_if_vectors partition_vecs

let leq_partition ty1 ty2 = Ty.diff ty1 ty2 |> partition_vecs |> Ty.is_empty

(* Regroup *)

let regroup_vecs tc =
  if Tag.equal (TagComp.tag tc) Vec.tag then
    let ty = Descr.mk_tagcomp tc |> Ty.mk_descr in
    let t = Vec.destruct ty in
    let tsimpl, t = t |> List.partition_map
      (function (p,[]) -> Either.left p | x -> Either.right x) in
    let tvector, tscalar = tsimpl |> List.partition_map
      (function Vec.Vector a -> Either.left a | Scalar a -> Either.right a) in
    let tvector, tscalar = Vec.mk (Vector (Ty.disj tvector)), Vec.mk (Scalar (Ty.disj tscalar)) in
    let ty = Ty.disj (tvector::tscalar::(List.map Vec.mk_line t)) in
    Ty.get_descr ty |> Descr.get_tags |> Tags.get Vec.tag
  else tc
let regroup_vecs tags =
  let b,tags = Tags.destruct tags in
  let tags = tags |> List.map regroup_vecs in
  Tags.construct (b,tags)
let regroup_vecs d =
  Descr.set_component d (Descr.Tags (Descr.get_tags d |> regroup_vecs))
let regroup_vecs vd =
  VDescr.map regroup_vecs vd
let regroup_vecs = transform_if_vectors regroup_vecs
