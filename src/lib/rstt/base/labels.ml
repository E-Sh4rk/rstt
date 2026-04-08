open Sstt

type t = Pos of int | Named of string | Sym of t

let labels = Hashtbl.create 100
let info = Hashtbl.create 100

let rec name t =
  match t with
  | Pos i -> Format.asprintf "%i" (i+1)
  | Named str -> str
  | Sym t -> let str = name t in "#"^str
let rec of_name str =
  if String.starts_with ~prefix:"#" str then
    Sym (String.sub str 1 (String.length str - 1) |> of_name)
  else
    match int_of_string_opt str with
    | Some i -> Pos (i-1)
    | None -> Named str
let get t =
  match Hashtbl.find_opt labels t with
  | Some lbl -> lbl
  | None ->
    let l = Label.mk (name t) in
    Hashtbl.add labels t l ; Hashtbl.add info l t ; l
let pos i = get (Pos i)
let named str = get (Named str)
let sym sym = get (Sym sym)
let info lbl =
  try Hashtbl.find info lbl with Not_found -> invalid_arg "Label is not a R label."
let is_sym lbl = match info lbl with Sym _ -> true | Named _ | Pos _ -> false

let id = Label.mk "_id"
let npos = Label.mk "_npos"

(* Symbolic label utilities *)

let to_sym lbl =
  match info lbl with
  | Sym t -> Some t
  | Pos _ | Named _ -> None
  | exception (Invalid_argument _) -> None
let labels_of_ty t =
  let labels = ref LabelSet.empty in
  let _ = Ty.nodes t |> List.iter (fun n ->
      Ty.def n |> VDescr.map (fun d ->
        let _ = d |> Descr.get_records |> Records.map (fun r ->
            labels := LabelSet.union !labels (Records.Atom.dom r) ; r
        ) in d
      ) |> ignore
    ) in !labels
let sym_of_ty ty =
  labels_of_ty ty |> LabelSet.elements |> List.filter_map to_sym

let substitute ~sym:from ~target ty =
  let from_lbl = sym from in
  let to_lbl = get target in
  let aux r =
    r |> Records.map (fun ra ->
        let dom = Records.Atom.dom ra in
        let bindings = ra.Records.Atom.bindings |> LabelMap.to_list |> List.map (fun (lbl,fty) ->
          if Label.equal lbl from_lbl && not (LabelSet.mem to_lbl dom)
          then (to_lbl, fty) else (lbl, fty)
          ) |> LabelMap.of_list in
        { Records.Atom.bindings ; tail=ra.Records.Atom.tail }      
      )
  in
  let aux d =
    Descr.set_component d (Descr.Records (Descr.get_records d |> aux))
  in
  let aux vd =
    VDescr.map aux vd
  in
  Transform.transform aux ty
