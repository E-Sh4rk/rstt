open Sstt

type t = Pos of int | Named of string

let compare t1 t2 = Stdlib.compare t1 t2
let equal t1 t2 = Stdlib.(=) t1 t2

let labels = Hashtbl.create 100
let info = Hashtbl.create 100

let name t =
  match t with
  | Pos i -> Format.asprintf "%i" (i+1)
  | Named str -> str

let of_name str =
  match int_of_string_opt str with
  | Some i -> Pos (i-1)
  | None -> Named str

let get t =
  match Hashtbl.find_opt labels t with
  | Some lbl -> lbl
  | None ->
    let l = Label.mk (name t |> Utils.slugify) in
    Hashtbl.add labels t l ; Hashtbl.add info l t ; l
let pos i = get (Pos i)
let named str = get (Named str)
let info lbl =
  try Hashtbl.find info lbl with Not_found -> invalid_arg "Label is not a R label."

module Reserved = struct
  let id = Label.mk "_id"
  let npos = Label.mk "_npos"
  let pos = Label.mk "_pos"
  let named = Label.mk "_named"
  let elt = Label.mk "_e"
  let content = Label.mk "_c"
  let classes = Label.mk "_class"
  let attrs = Label.mk "_attr"
  let target = Label.mk "_target"
end
