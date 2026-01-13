open Sstt

type t = Pos of int | Named of string

let labels = Hashtbl.create 100
let info = Hashtbl.create 100

let name t =
  match t with
  | Pos i -> Format.asprintf "%i" i
  | Named str -> str
let get t =
  match Hashtbl.find_opt labels t with
  | Some lbl -> lbl
  | None ->
    let l = Label.mk (name t) in
    Hashtbl.add labels t l ; Hashtbl.add info l t ; l
let pos i = get (Pos i)
let named str = get (Named str)
let info lbl =
  try Hashtbl.find info lbl with Not_found -> invalid_arg "Label is not a R label."
