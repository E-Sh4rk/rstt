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

let print_builtin fmt b =
  let str =
    match b with
    | Printer.Empty -> "empty"
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

let pp_z = Z.pp_print
let print_interval fmt (lb,ub) =
  match lb, ub with
  | None, None -> print_builtin fmt Printer.AnyInt
  | Some lb, Some ub when Z.equal lb ub ->
    Format.fprintf fmt "%a" pp_z lb
  | Some lb, Some ub ->
    Format.fprintf fmt "(%a..%a)" pp_z lb pp_z ub
  | None, Some ub ->
    Format.fprintf fmt "(..%a)" pp_z ub
  | Some lb, None ->
    Format.fprintf fmt "(%a..)" pp_z lb

let rec print_descr_ctx prec assoc fmt d =
  let rec aux prec assoc fmt d =
    let open Format in
    match d.Printer.op with
    | Extension e ->
      fprintf fmt "%a" (Printer.print_extension_node_ctx prec assoc) e
    | Alias str -> fprintf fmt "%s" str
    | Node n -> fprintf fmt "%a" Printer.NodeId.pp n
    | Builtin b -> print_builtin fmt b
    | Var v -> fprintf fmt "%a" Var.pp v
    | Enum a -> fprintf fmt "%a" Enum.pp a
    | Tag (t,d) ->
      fprintf fmt "%a(%a)"
        Tag.pp t print_descr d
    | Interval (lb,ub) -> fprintf fmt "%a" print_interval (lb,ub)
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
    | Varop (v,ds) ->
      Prec.print_nary_op aux prec assoc v fmt ds
    | Binop (b,d1,d2) ->
      let sym,prec',_ as opinfo = Prec.binop_info b in
      Prec.fprintf prec assoc opinfo fmt "%a%(%)%a"
        (aux prec' Left) d1 sym
        (aux prec' Right) d2
    | Unop (u,d) ->
      let sym,prec',_ as opinfo = Prec.unop_info u in
      Prec.fprintf prec assoc opinfo fmt "%(%)%a" sym (aux prec' NoAssoc) d
  in
  aux prec assoc fmt d

and print_fop prec assoc fmt fop =
  let rec aux prec assoc fmt fop =
    match fop with
    | Printer.FRowVar v -> Format.fprintf fmt "%a" RowVar.pp v
    | FTy (d, opt) ->
      if opt then
        Format.fprintf fmt "%a?" (print_descr_ctx Prec.max_prec NoAssoc) d
      else
        print_descr_ctx prec assoc fmt d
    | FVarop (FCup,ds) -> print_cup ~cmp:(Compare.fop Compare.descr) aux prec assoc fmt ds
    | FVarop (FCap,ds) -> print_cap ~cmp:(Compare.fop Compare.descr) aux prec assoc fmt ds
    | FBinop (b,fop1,fop2) ->
      let sym,prec',_ as opinfo = Prec.fbinop_info b in
      Prec.fprintf prec assoc opinfo fmt "%a%(%)%a"
        (aux prec' Left) fop1 sym
        (aux prec' Right) fop2
    | FUnop (u,fop) ->
      let sym,prec',_ as opinfo = Prec.funop_info u in
      Prec.fprintf prec assoc opinfo fmt "%(%)%a" sym (aux prec' NoAssoc) fop
  in
  aux prec assoc fmt fop

and print_tail fmt tail =
  match tail with
  | FTy ({ op=Builtin Any ; _ }, true) -> Format.fprintf fmt ".."
  | FTy ({ op=Builtin Empty ; _ }, true) -> Format.fprintf fmt ""
  | _ -> Format.fprintf fmt ";;@ %a@ " print_fop' tail

and print_descr fmt d = print_descr_ctx Prec.min_prec NoAssoc fmt d
and print_fop' fmt fop = print_fop Prec.min_prec NoAssoc fmt fop
let print_descr_atomic = print_descr_ctx Prec.max_prec Prec.NoAssoc
let print fmt t =
  assert (t.Printer.defs = []) ;
  print_descr fmt t.main
let print_field_ctx = print_fop

(* ========================= *)

let ty' aliases fmt t =
  let t = Printer.get ~factorize:false (printer_params' aliases) t in
  print fmt t
let ty = ty' []
let subst' aliases fmt s =
  Printer.print_subst (printer_params' aliases) fmt s
let subst = subst' []