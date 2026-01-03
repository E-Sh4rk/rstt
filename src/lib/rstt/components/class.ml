open Sstt

let h = Extensions.Hierarchy.new_hierarchy ()
let nodes = Hashtbl.create 10

let define_class str sub =
  if Hashtbl.mem nodes str then invalid_arg "Class already defined" ;
  try
    let subnodes = sub |> List.map (fun str -> Hashtbl.find nodes str) in
    let n = Extensions.Hierarchy.new_node h ~name:str ~subnodes in
    Hashtbl.add nodes str n
  with Not_found -> invalid_arg "Undefined subclass"

let mk str =
  match Hashtbl.find_opt nodes str with
  | None -> invalid_arg "Undefined class"
  | Some n -> Extensions.Hierarchy.mk h n

let () = Pp.add_printer_param (Extensions.Hierarchy.printer_params h)
