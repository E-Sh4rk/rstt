open Sstt
open Rstt_utils

type t = Pos of int | Named of string | Sym of sym
and sym = SLabel of t list | SStr of string

let compare t1 t2 = Stdlib.compare t1 t2
let equal t1 t2 = Stdlib.(=) t1 t2

let labels = Hashtbl.create 100
let info = Hashtbl.create 100

let rec name t =
  match t with
  | Pos i -> Format.asprintf "%i" (i+1)
  | Named str -> str
  | Sym sym -> "#"^(sym_name sym)
and sym_name sym =
  match sym with
  | SStr str -> str
  | SLabel ts ->
    let strs = List.map name ts in
    Format.asprintf "(%a)" (print_seq Format.pp_print_string ",") strs

let rec of_name str =
  if String.starts_with ~prefix:"#" str then
    Sym (String.sub str 1 (String.length str - 1) |> sym_of_name)
  else
    match int_of_string_opt str with
    | Some i -> Pos (i-1)
    | None -> Named str
and sym_of_name str =
  if String.starts_with ~prefix:"(" str
  then
    let str = String.sub str 1 (String.length str - 2) in
    SLabel (String.split_on_char ',' str |> List.map of_name)
  else SStr str

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

module Reserved = struct
  let id = Label.mk "_id"
  let npos = Label.mk "_npos"
  let pos = Label.mk "_pos"
  let named = Label.mk "_named"
  let card = Label.mk "_n"
  let content = Label.mk "_c"
  let classes = Label.mk "_class"
  let attrs = Label.mk "_attr"
  let target = Label.mk "_target"
end

(* Symbolic label utilities *)

let to_sym lbl =
  match info lbl with
  | Sym s -> Some s
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

type sym_selector = SelectLabel of t | SelectString of string
type sym_subst = { selector:sym_selector ; target:t }
let select selector sym =
  match sym, selector with
  | SStr str, SelectString str' -> String.equal str str'
  | SLabel ts, SelectLabel t -> List.exists (equal t) ts
  | _, _ -> false
let substitute lst ty =
  let lst = lst |> List.map (fun {selector;target} -> (select selector, get target)) in
  let aux r =
    r |> Records.map (fun ra ->
        let dom = Records.Atom.dom ra in
        let bindings = ra.Records.Atom.bindings |> LabelMap.to_list |> List.map (fun (lbl,fty) ->
            match to_sym lbl with
            | None -> (lbl,fty)
            | Some sym ->
              begin match List.find_opt (fun (f,target) -> f sym && not (LabelSet.mem target dom)) lst with
              | None -> (lbl, fty)
              | Some (_, target) -> (target, fty)
              end
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
