open Sstt

let enums = Hashtbl.create 256
let strings = Hashtbl.create 256
let is_alphanum = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false
let slugify = String.map (fun c -> if is_alphanum c then c else '_')
let enum str =
  match Hashtbl.find_opt enums str with
  | Some atom -> atom
  | None ->
    let atom = Enum.mk ("_"^(slugify str)) in
    Hashtbl.add enums str atom ;
    Hashtbl.add strings atom str ;
    atom
let string enum = enum |> Hashtbl.find strings
