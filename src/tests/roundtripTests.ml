(* Property-based tests of the printer against the parser: printing a type and
   parsing the text back should give the same type, and so should printing and
   parsing that type again.

   The types are generated as the parser's AST ([Rstt_repl.Ast.ty], i.e.
   [Builder.t] over names), so that they cover what the printer can be asked
   to print even when no syntax writes it yet (e.g. a field name that is not
   an identifier). For each generated [ast]:

   1. [print (build ast)] parses;
   2. the type it parses to is equivalent to [build ast];
   3. printing that type gives a text that parses to an equivalent type.

   Property 3 used to be stronger: printing that type gives the same text.
   It does not hold, and is left aside (see [round_trip]): how sstt's printer
   splits a type into nodes (which parts it groups, writes as a difference,
   or names [lgl1]) depends on how the type is represented, i.e. on how it
   was built, not only on what it means. So the same type can print as
   [CHR1 | INT1 \ ^INT1 | RAW1] or [(CHR1 | RAW1) | INT1 \ ^INT1].

   The argument lists of signatures get a fresh identity each time they are
   built ([Arg.mk]), which the printer does not print: equivalence (2) is
   checked on both types built with polymorphic identities ([TPolyArg]),
   compared by tallying with those identities instantiable. *)

open Rstt
open Rstt_repl

module Gen = QCheck2.Gen

type ast = Ast.ty

(* ===== Generators ===== *)

(* Names used for fields and parameters: identifiers, keywords the parser
   accepts as labels, and names R allows but that are not identifiers. *)
let label =
  Gen.oneof_weighted [
    8, Gen.oneof_list [ "a" ; "b" ; "x" ; "y" ; "data.frame" ; "na.rm" ] ;
    1, Gen.oneof_list [ "type" ; "with" ; "class" ] ;
    1, Gen.oneof_list [ "Agglomerative coefficient" ; "$date" ; "vctrs:::s3_fallback" ] ]

let var_name = Gen.oneof_list [ "'a" ; "'b" ]

(* The classes the generated types mention: declared in [declare_classes]. *)
let class_names = [ "pbt_cls1" ; "pbt cls2" ]

let interval =
  let bound = Gen.int_range (-3) 50 in
  Gen.oneof_weighted [
    3, Gen.map2 (fun a b -> let a, b = min a b, max a b in Some a, Some b) bound bound ;
    2, Gen.map (fun b -> None, Some b) bound ;
    2, Gen.map (fun a -> Some a, None) bound ;
    1, Gen.return (None, None) ]

let rec prim depth : string Builder.prim Gen.t =
  let open Builder in
  let leaf = Gen.oneof_weighted [
    6, Gen.oneof_list [ PLgl ; PChr ; PInt ; PDbl ; PClx ; PRaw ; PNum ] ;
    3, Gen.oneof_list [ PSubLgl ; PSubChr ; PSubInt ; PSubDbl ; PSubClx ; PSubRaw ] ;
    2, Gen.map (fun i -> PInt' i) interval ;
    2, Gen.map (fun i -> PDbl' i) interval ;
    1, Gen.map (fun i -> PNum' i) interval ;
    1, Gen.map (fun s -> PChr' s) (Gen.oneof_list [ "abc" ; "a b" ]) ;
    1, Gen.map (fun b -> PLgl' b) Gen.bool ;
    1, Gen.map (fun v -> PIntVar v) var_name ;
    1, Gen.map (fun v -> PChrVar v) var_name ;
    1, Gen.map (fun v -> PDblVar v) var_name ]
  in
  if depth <= 0 then leaf
  else
    let sub = prim (depth - 1) in
    Gen.oneof_weighted [
      5, leaf ;
      2, Gen.map (fun p -> PHat p) sub ;
      1, Gen.map2 (fun p q -> PCup (p, q)) sub sub ;
      1, Gen.map2 (fun p q -> PDiff (p, q)) sub sub ;
      1, Gen.map (fun p -> PNeg p) sub ]

let vec depth : ast Gen.t =
  Gen.map2 (fun scalar p -> Builder.TVec (if scalar then Scalar p else Vector p))
    Gen.bool (prim depth)

let cconst : ast Gen.t =
  let open Builder in
  Gen.map (fun c -> TCConst c) (Gen.oneof_weighted [
    3, Gen.oneof_list [ CInt ; CIntNa ; CString ; CDouble ; CBool ; CTrue ; CNull ; CNa ] ;
    2, Gen.map (fun i -> CIntSingl i) (Gen.int_range (-3) 50) ;
    3, Gen.map (fun i -> CIntInterval i) interval ;
    1, Gen.map (fun s -> CStrSingl s) (Gen.oneof_list [ "abc" ; "a b" ]) ])

let basic : ast Gen.t =
  let open Builder in
  Gen.oneof_list [ TAny ; TEmpty ; TNull ; TEnv ; TSym ; TLang ; TExtPtr ]

(* Distinct labels, each with a generated value. *)
let fields n g =
  Gen.list_size (Gen.int_bound n) (Gen.pair label g)
  |> Gen.map (List.fold_left (fun acc (l, t) ->
      if List.mem_assoc l acc then acc else acc @ [ l, t ]) [])

let absent : ast = Builder.TOption TEmpty

let rec ty depth : ast Gen.t =
  let open Builder in
  if depth <= 0 then
    Gen.oneof_weighted [ 3, basic ; 5, vec 1 ; 2, cconst ; 1, Gen.map (fun v -> TVar v) var_name ]
  else
    let sub = ty (depth - 1) in
    let field = Gen.oneof_weighted [ 3, sub ; 1, Gen.map (fun t -> TOption t) sub ] in
    Gen.oneof_weighted [
      4, ty 0 ;
      2, vec depth ;
      1, Gen.map2 (fun a b -> TCup (a, b)) sub sub ;
      1, Gen.map2 (fun a b -> TCap (a, b)) sub sub ;
      1, Gen.map2 (fun a b -> TDiff (a, b)) sub sub ;
      1, Gen.map (fun a -> TNeg a) sub ;
      1, Gen.map (fun ts -> TTuple ts) (Gen.list_size (Gen.int_bound 3) sub) ;
      (* Records, closed or open. *)
      2, Gen.map2 (fun bindings tl -> TList { bindings ; tl })
           (fields 3 field) (Gen.oneof_list [ absent ; TOption TAny ]) ;
      (* Signatures: named parameters, maybe a [...]. *)
      2, Gen.map3 (fun pos_named tl ret ->
           let pos_tl, named_tl = match tl with None -> absent, absent | Some t -> t, t in
           TArrow (TArg { pos_named ; pos_tl ; named = [] ; named_tl }, ret))
           (fields 3 field) (Gen.option sub) sub ;
      (* Attributes: no class, or a class. *)
      1, Gen.map (fun content -> TAttr { content ; classes = CNoClass ; attrs = TAny }) sub ;
      1, Gen.map2 (fun content cls ->
           TAttr { content ;
                   classes = CClasses { pos = [ Classes.L (Class cls, []) ] ; neg = [] ;
                                        unk = [] ; tail = Unknown } ;
                   attrs = TAny })
           sub (Gen.oneof_list class_names) ;
      1, Gen.map (fun t -> TCPtr t) sub ;
      (* A recursive list type: x = leaf | { a: x }. *)
      1, Gen.map (fun leaf ->
           TWhere (TId "x", [ "x", TCup (leaf, TList { bindings = [ "a", TId "x" ] ;
                                                     tl = absent }) ]))
           (ty 0) ]

(* ===== The property ===== *)

let declare_classes =
  let declared = ref false in
  fun () ->
    if not !declared then begin
      List.iter (fun c -> Classes.declare_class c ~methods:[ "print" ]) class_names ;
      declared := true
    end

let print env t =
  let b = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt 1_000_000 ;
  Format.pp_set_max_indent fmt 999_999 ;
  Format.fprintf fmt "@[<h>%a@]@?" (Pp.ty' (Ast.aliases env)) (TyOp.simplify t) ;
  String.concat " " (String.split_on_char '\n' (Buffer.contents b))

(* The argument lists, with polymorphic identities. *)
let poly_args (a : ast) : ast =
  Builder.map (function Builder.TArg a -> Builder.TPolyArg a | t -> t) Fun.id Fun.id a

(* Equivalence, the identities of the argument lists being instantiable (cf.
   the REPL's [poly_leq]; the generated variables are all monomorphic). *)
let equiv t1 t2 =
  let ids = Arg.polymorphic_vars () in
  let leq a b =
    let delta = MixVarSet.union (Ty.all_vars a) (Ty.all_vars b)
      |> MixVarSet.filter (fun v -> not (VarSet.mem v ids)) (fun _ -> true) in
    Tallying.tally delta [ a, b ] |> List.is_empty |> not
  in
  leq t1 t2 && leq t2 t1

type outcome =
  | Ok
  | Unprintable of string                    (* the printer raised *)
  | Unparsable of string * string            (* printed text, error *)
  | Different of string * string             (* printed, reprinted *)
  | Unstable of string * string              (* printed, reprinted, which does not
                                                parse to an equivalent type *)

let round_trip (a : ast) =
  declare_classes () ;
  let t0, env = Ast.build_ty Ast.empty_env a in
  match print env t0 with
  | exception e -> Unprintable (Printexc.to_string e)
  | p1 ->
    match IO.parse_type p1 with
    | exception e -> Unparsable (p1, Printexc.to_string e)
    | a1 ->
      match Ast.build_ty env a1 with
      | exception e -> Unparsable (p1, Printexc.to_string e)
      | t1, env ->
        let p2 = print env t1 in
        let t0p, env = Ast.build_ty env (poly_args a) in
        let t1p, env = Ast.build_ty env (poly_args a1) in
        if not (equiv t0p t1p) then Different (p1, p2)
        else
          match IO.parse_type p2 with
          | exception _ -> Unstable (p1, p2)
          | a2 ->
            match Ast.build_ty env (poly_args a2) with
            | exception _ -> Unstable (p1, p2)
            | t2p, _ -> if not (equiv t1p t2p) then Unstable (p1, p2) else Ok
          (* The text itself is not stable (cf. the header):
             else if p1 <> p2 then Unstable (p1, p2) *)

(* A generated type is kept only if it builds: some combinations are not
   types (e.g. a recursive definition that is not well-founded). *)
let buildable a =
  declare_classes () ;
  match Ast.build_ty Ast.empty_env a with _ -> true | exception _ -> false

let describe a =
  declare_classes () ;
  match Ast.build_ty Ast.empty_env a with
  | exception e -> "unbuildable: " ^ Printexc.to_string e
  | t, env ->
    let shown = try print env t with e -> "unprintable: " ^ Printexc.to_string e in
    match round_trip a with
    | Ok -> shown
    | Unprintable e -> shown ^ "\n  printing failed: " ^ e
    | Unparsable (p, e) -> p ^ "\n  does not parse: " ^ e
    | Different (p1, p2) -> p1 ^ "\n  reparses to a different type, printed: " ^ p2
    | Unstable (p1, p2) ->
      p1 ^ "\n  reprints as: " ^ p2 ^ ", which does not parse back to the same type"

let test =
  QCheck2.Test.make ~name:"print then parse is the identity" ~count:500 ~print:describe
    (Gen.sized_size (Gen.int_bound 3) ty)
    (fun a -> QCheck2.assume (buildable a) ; round_trip a = Ok)

let%test_unit "round trip of the printer and the parser" =
  QCheck2.Test.check_exn ~rand:(Random.State.make [| 42 |]) test
