open Sstt

module Compare = struct
  open Printer
  open Prec

  let rec fop : 'a. ('a -> 'a -> int) -> 'a fop -> 'a fop -> int =
    fun cmp_d a b ->
    match a, b with
    | FVarop (fv1, l1), FVarop (fv2, l2) ->
      (match Stdlib.compare fv1 fv2 with
       | 0 -> List.compare (fop cmp_d) l1 l2
       | i -> i)
    | FBinop (fb1, a1, b1), FBinop (fb2, a2, b2) ->
      (match Stdlib.compare fb1 fb2 with
       | 0 -> (match fop cmp_d a1 a2 with 0 -> fop cmp_d b1 b2 | i -> i)
       | i -> i)
    | FUnop (fu1, a1), FUnop (fu2, a2) ->
      (match Stdlib.compare fu1 fu2 with 0 -> fop cmp_d a1 a2 | i -> i)
    | FTy (d1, b1), FTy (d2, b2) ->
      (match cmp_d d1 d2 with 0 -> Bool.compare b1 b2 | i -> i)
    | FRowVar r1, FRowVar r2 -> RowVar.compare r1 r2
    | FVarop _, _ -> -1 | _, FVarop _ -> 1
    | FBinop _, _ -> -1 | _, FBinop _ -> 1
    | FUnop _, _ -> -1 | _, FUnop _ -> 1
    | FTy _, _ -> -1 | _, FTy _ -> 1

  let builtin a b =
    match a, b with
    | AnyTupleComp i1, AnyTupleComp i2 -> Stdlib.compare i1 i2
    | AnyTagComp t1, AnyTagComp t2 -> Tag.compare t1 t2
    | _ -> Stdlib.compare a b

  let rec descr a b = op a.op b.op

  and op a b =
    match a, b with
    | Extension e1, Extension e2 ->
      String.compare
        (Format.asprintf "%a" (print_extension_node_ctx min_prec NoAssoc) e1)
        (Format.asprintf "%a" (print_extension_node_ctx min_prec NoAssoc) e2)
    | Alias s1, Alias s2 -> String.compare s1 s2
    | Node n1, Node n2 -> NodeId.compare n1 n2
    | Builtin b1, Builtin b2 -> builtin b1 b2
    | Var v1, Var v2 -> Var.compare v1 v2
    | Enum e1, Enum e2 -> Enum.compare e1 e2
    | Tag (t1, d1), Tag (t2, d2) ->
      (match Tag.compare t1 t2 with 0 -> descr d1 d2 | i -> i)
    | Interval (lo1, hi1), Interval (lo2, hi2) ->
      (match Option.compare Z.compare lo1 lo2 with
       | 0 -> Option.compare Z.compare hi1 hi2
       | i -> i)
    | Record (fields1, rest1), Record (fields2, rest2) ->
      let cmp_field (l1, f1) (l2, f2) =
        match Label.compare l1 l2 with 0 -> fop descr f1 f2 | i -> i
      in
      (match List.compare cmp_field fields1 fields2 with
       | 0 -> fop descr rest1 rest2
       | i -> i)
    | Varop (v1, ds1), Varop (v2, ds2) ->
      (match Stdlib.compare v1 v2 with
       | 0 -> List.compare descr ds1 ds2
       | i -> i)
    | Binop (b1, a1, c1), Binop (b2, a2, c2) ->
      (match Stdlib.compare b1 b2 with
       | 0 -> (match descr a1 a2 with 0 -> descr c1 c2 | i -> i)
       | i -> i)
    | Unop (u1, d1), Unop (u2, d2) ->
      (match Stdlib.compare u1 u2 with 0 -> descr d1 d2 | i -> i)
    | Extension _, _ -> -1 | _, Extension _ -> 1
    | Alias _, _ -> -1 | _, Alias _ -> 1
    | Node _, _ -> -1 | _, Node _ -> 1
    | Builtin _, _ -> -1 | _, Builtin _ -> 1
    | Var _, _ -> -1 | _, Var _ -> 1
    | Enum _, _ -> -1 | _, Enum _ -> 1
    | Tag _, _ -> -1 | _, Tag _ -> 1
    | Interval _, _ -> -1 | _, Interval _ -> 1
    | Record _, _ -> -1 | _, Record _ -> 1
    | Varop _, _ -> -1 | _, Varop _ -> 1
    | Binop _, _ -> -1 | _, Binop _ -> 1
end

let pparams = ref Printer.empty_params

let add_printer_param p = pparams := Printer.merge_params [!pparams ; p]
let printer_params' aliases = { !pparams with aliases=aliases }
let printer_params () = printer_params' []

let print_cup ~cmp f prec assoc fmt vs =
  let vs = List.sort cmp vs in
  Prec.print_cup f prec assoc fmt vs

let print_cap ~cmp f prec assoc fmt vs =
  let vs = List.sort cmp vs in
  Prec.print_cap f prec assoc fmt vs

(* let print_line ~any ~cmp f prec assoc fmt (ps,ns) =
  let ps, ns = List.sort cmp ps, List.sort cmp ns in
  Prec.print_line ~any f prec assoc fmt (ps,ns) *)

let print_non_empty_dnf ~any ~cmp f prec assoc fmt dnf =
  let sort_line (ps,ns) = List.sort cmp ps, List.sort cmp ns in
  let dnf = List.map sort_line dnf in
  let cmp_line (ps1,ns1) (ps2,ns2) =
    match List.compare cmp ps1 ps2 with
    | 0 -> List.compare cmp ns1 ns2
    | i -> i
  in
  let dnf = List.sort cmp_line dnf in
  Prec.print_non_empty_dnf ~any f prec assoc fmt dnf

let print_dnf ~empty ~any ~cmp f prec assoc fmt dnf =
  match dnf with
  | [] -> Format.fprintf fmt "%s" empty
  | dnf -> print_non_empty_dnf ~any ~cmp f prec assoc fmt dnf

(* ===== Descr printer ===== *)

type printing_pos = Tl | Struct | Prim of string (* suffix *)
open Effect.Deep
open Effect
type _ Effect.t += GetPos: printing_pos t
let current_pos () =
  try perform GetPos with Unhandled GetPos -> Tl
let with_pos (pp:printing_pos) f t =
  match f t with
  | x -> x
  | effect GetPos, k -> continue k pp

let print_builtin fmt b =
  let open Sstt.Printer in
  let str =
    match b with
    | Empty -> "empty"
    | Any -> "any"
    | AnyTuple -> "tuple"
    | AnyEnum -> "enum"
    | AnyTag -> "tag"
    | AnyInt -> "int"
    | AnyArrow -> "arrow"
    | AnyRecord -> "record"
    | AnyTupleComp i -> "tuple"^(string_of_int i)
    | AnyTagComp t -> (Tag.name t)^("()")
  in
  Format.fprintf fmt "%s" str

let rec print_descr_ctx' pos prec assoc fmt d =
  let rec aux prec assoc fmt d =
    let open Format in
    match d.Printer.op with
    | Extension e -> Printer.print_extension_node_ctx prec assoc fmt e
    | Alias str -> fprintf fmt "%s" str
    | Node n -> fprintf fmt "%a" Printer.NodeId.pp n
    | Builtin b -> print_builtin fmt b
    | Var v ->
      begin match pos with
      | Prim suffix -> fprintf fmt "v%s(%a)" suffix Var.pp v
      | _ -> fprintf fmt "%a" Var.pp v
      end
    | Enum a -> fprintf fmt "%a" Enum.pp a
    | Tag (t,d) ->
      fprintf fmt "%a(%a)"
        Tag.pp t print_descr d
    | Interval (lb,ub) -> fprintf fmt "%a" Printer.print_interval (lb,ub)
    | Record (bindings,tail) ->
      let print_binding fmt (l,f) =
        Format.fprintf fmt "%a :@ %a"
          Label.pp l
          print_fop' f
      in
      Format.fprintf fmt "{@ %a@ %a}"
        (Prec.print_seq print_binding " ;@ ") bindings
        print_tail tail
    | Varop (Cup,ds) -> print_cup ~cmp:Compare.descr aux prec assoc fmt ds
    | Varop (Cap,ds) -> print_cap ~cmp:Compare.descr aux prec assoc fmt ds
    | Varop (Tuple,ds) ->
      let tpl fmt ds =
        Prec.print_nary_op print_tl_descr_ctx Prec.min_prec Prec.NoAssoc Tuple fmt ds
      in
      Format.fprintf fmt "[%a]" tpl ds
    | Binop (Diff,d1,d2) -> Prec.print_binary_op aux prec assoc Diff fmt d1 d2
    | Binop (Arrow,d1,d2) when pos=Struct ->
      Prec.print_binary_op print_tl_descr_ctx prec assoc Arrow fmt d1 d2
    | Binop (Arrow,d1,d2) ->
      let _,prec,assoc = Prec.binop_info Arrow in
      Prec.print_binary print_tl_descr_ctx prec assoc
        (Prec.fs "@ -->@ ",prec,assoc) fmt d1 d2
    | Unop (Neg,d) -> Prec.print_unary_op aux prec assoc Neg fmt d
  in
  with_pos pos (aux prec assoc fmt) d

and print_descr_ctx prec assoc fmt d =
  print_descr_ctx' (current_pos ()) prec assoc fmt d
and print_tl_descr_ctx prec assoc fmt d =
  print_descr_ctx' Tl prec assoc fmt d  
and print_struct_descr_ctx prec assoc fmt d =
  print_descr_ctx' Struct prec assoc fmt d
and print_prim_descr_ctx prec assoc fmt d =
  print_descr_ctx' (Prim "") prec assoc fmt d

and print_fop prec assoc fmt fop =
  let rec aux prec assoc fmt fop =
    match fop with
    | Printer.FRowVar v -> Format.fprintf fmt "%a" RowVar.pp v
    | FTy (d, opt) ->
      if opt then
        Format.fprintf fmt "%a?" (print_tl_descr_ctx Prec.max_prec NoAssoc) d
      else
        print_tl_descr_ctx prec assoc fmt d
    | FVarop (FCup,ds) -> print_cup ~cmp:(Compare.fop Compare.descr) aux prec assoc fmt ds
    | FVarop (FCap,ds) -> print_cap ~cmp:(Compare.fop Compare.descr) aux prec assoc fmt ds
    | FBinop (b,fop1,fop2) -> Prec.print_binary_fop aux prec assoc b fmt fop1 fop2
    | FUnop (u,fop) -> Prec.print_unary_fop aux prec assoc u fmt fop
  in
  aux prec assoc fmt fop

and print_tail fmt tail =
  match tail with
  | FTy ({ op=Builtin Any ; _ }, true) -> Format.fprintf fmt ".."
  | FTy ({ op=Builtin Empty ; _ }, true) -> Format.fprintf fmt ""
  | _ -> Format.fprintf fmt ";;@ %a@ " print_fop' tail

and print_descr fmt d = print_tl_descr_ctx Prec.min_prec NoAssoc fmt d
and print_fop' fmt fop = print_fop Prec.min_prec NoAssoc fmt fop
let print_descr_atomic = print_tl_descr_ctx Prec.max_prec Prec.NoAssoc
let print_field_ctx = print_fop


let print_def fmt (n,d) =
  Format.fprintf fmt "%a =@ %a" Printer.NodeId.pp n print_descr d
let print fmt t =
  Format.fprintf fmt "%a" print_descr t.Printer.main ;
  match t.defs with
  | [] -> ()
  | defs ->
    Format.fprintf fmt "@ where@ %a" (Prec.print_seq print_def "@ and@ ") defs


let pp_struct_tag f _ _ fmt t =
  Format.fprintf fmt "s(%a)" (f Prec.min_prec Prec.NoAssoc) t

let pp_prim_tag f _ _ fmt t =
  Format.fprintf fmt "p(%a)" (f Prec.min_prec Prec.NoAssoc) t


(* ========================= *)

let ty' aliases fmt t =
  let t = Printer.get ~factorize:false (printer_params' aliases) t in
  print fmt t
let ty = ty' []

let row' aliases fmt r =
  let bindings, tail = Row.bindings r, Row.tail r in
  let tail, fields, defs =
    match Printer.get_field' (printer_params' aliases) (tail::List.map snd bindings) with
    | { main=tl::bindings ; defs } -> tl, bindings, defs
    | _ -> assert false
  in
  let bindings = List.combine (List.map fst bindings) fields in
  let ast = { Printer.main={ Printer.ty=Ty.any ; op=Record (bindings, tail) } ; defs } in
  print fmt ast
let row = row' []

let subst' aliases fmt s =
  let print_ty, print_row = ty' aliases, row' aliases in
  let pp_binding1 fmt (v,ty) =
    Format.fprintf fmt "@,@[<hov>%a: %a@]" Var.pp v print_ty ty
  in
  let pp_binding2 fmt (v,r) =
    Format.fprintf fmt "@,@[<hov>%a: %a@]" RowVar.pp v print_row r
  in
  let pp_binding' fmt b =
    match b with
    | `T (v,ty) -> pp_binding1 fmt (v,ty)
    | `R (v,r) -> pp_binding2 fmt (v,r)
  in
  let b1 = Subst.bindings1 s |> List.map (fun b -> `T b) in
  let b2 = Subst.bindings2 s |> List.map (fun b -> `R b) in
  Format.fprintf fmt "@[<v 0>[[@[<v 1>%a@]@,]]@]"
    (Prec.print_seq pp_binding' " ;") (b1@b2)
let subst = subst' []
