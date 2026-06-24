open Sstt

let enums = Hashtbl.create 256
let strings = Hashtbl.create 256
let enum str =
  match Hashtbl.find_opt enums str with
  | Some atom -> atom
  | None ->
    let atom = Enum.mk (Utils.slugify str) in
    Hashtbl.add enums str atom ;
    Hashtbl.add strings atom str ;
    atom
let string enum = enum |> Hashtbl.find strings
