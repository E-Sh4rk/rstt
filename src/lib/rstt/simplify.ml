open Sstt

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
  let has_vec = ref false in
  let _ = ty |> Ty.nodes |> List.map (fun ty ->
    Ty.def ty |> VDescr.map (fun d ->
      let (tags,_) = Descr.get_tags d |> Tags.components in
      if List.exists (fun tc -> TagComp.tag tc |> Tag.equal Vec.tag) tags
      then has_vec := true ;
      d
    )) in
  if !has_vec then Transform.transform partition_vecs ty else ty

let leq_partition ty1 ty2 = Ty.diff ty1 ty2 |> partition_vecs |> Ty.is_empty

let simplify = Transform.simplify ~normalize:partition_vecs
