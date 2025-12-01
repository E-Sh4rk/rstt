
let print_seq f sep =
  Format.(pp_print_list  ~pp_sep:(fun fmt () -> pp_print_string fmt sep) f)

let print_seq_cut f =
  Format.(pp_print_list ~pp_sep:pp_print_cut f)

let print_seq_space f =
  Format.(pp_print_list ~pp_sep:pp_print_space f)

(* MISC *)

let[@inline always] ccmp f e1 e2 r =
  if r <> 0 then r else f e1 e2

(* LISTS *)

(* let take_one lst =
  let[@tail_mod_cons] rec loop acc = function
      [] -> []
    | e :: lst -> (e, List.rev_append acc lst)::loop(e::acc) lst
  in loop [] lst *)

let cartesian_product l1 l2 =
  let rec loop l1 acc =
    match l1 with
    | [] -> acc
    | e1::l1 -> loop_one e1 l2 l1 acc
  and loop_one e1 l2 l1 acc =
    match l2 with
    | [] -> loop l1 acc
    | e2::l2 -> loop_one e1 l2 l1 ((e1, e2)::acc)
  in
  loop l1 [] |> List.rev

(* let rec cartesian_products lst =
  match lst with
  | [] -> [[]]
  | e::lst ->
    cartesian_products lst |> cartesian_product e
    |> List.map (fun (e1, e2) -> e1::e2) *)

let fold_acc_rem f lst =
  let rec aux acc rem =
    match rem with
    | [] -> acc
    | c::rem -> aux (f c acc rem) rem
  in
  aux [] lst

let filter_among_others pred lst =
  lst |> fold_acc_rem (fun c acc rem ->
    if pred c (List.rev_append acc rem) then c::acc else acc)
  |> List.rev

let map_among_others f lst =
  lst |> fold_acc_rem (fun c acc rem ->
    (f c (List.rev_append acc rem))::acc)
  |> List.rev

let merge_when_possible merge_opt lst =
  let rec find_map_in_tail acc e l =
    match l with
    | [] -> None
    | e' :: l -> match merge_opt e e' with
        None -> find_map_in_tail (e' :: acc) e l
      | Some a -> Some (a :: List.rev_append acc l)
  in
  let rec aux lst =
    match lst with
      [] -> []
    | e :: lst -> match find_map_in_tail [] e lst with
        None -> e :: aux lst
      | Some l -> aux l
  in aux lst
