open Sstt

type t = Pos of int | Named of string | PosNamed of int * string

let labels = Hashtbl.create 100
let info = Hashtbl.create 100

let name t =
  match t with
  | Pos i -> Format.asprintf "%i" i
  | Named str -> str
  | PosNamed (i,str) -> Format.asprintf "%i_%s" i str
let get t =
  match Hashtbl.find_opt labels t with
  | Some lbl -> lbl
  | None ->
    let l = Label.mk (name t) in
    Hashtbl.add labels t l ; Hashtbl.add info l t ; l
let pos i = get (Pos i)
let named str = get (Named str)
let pos_named (i,str) = get (PosNamed (i,str))
let info lbl =
  try Hashtbl.find info lbl with Not_found -> invalid_arg "Label is not a R label."
