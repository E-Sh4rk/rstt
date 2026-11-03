(* In safe mode, printer extensions whose decoding may lose information check
   that the decoded value still denotes the printed type, and otherwise leave it
   to the generic printer. *)
let safe_mode = ref false

let bool name r str =
  match String.lowercase_ascii str with
  | "true" | "on" -> r := true
  | "false" | "off" -> r := false
  | _ -> invalid_arg (Printf.sprintf "Option %s expects a boolean (true/false), got %s" name str)

(* Settings that can be changed by name, e.g. from the REPL *)
let options = [
  "safe_mode", bool "safe_mode" safe_mode ;
]

let set name value =
  match List.assoc_opt name options with
  | Some set -> set value
  | None ->
    invalid_arg (Printf.sprintf "Unknown option %s (available: %s)" name
      (options |> List.map fst |> String.concat ", "))
