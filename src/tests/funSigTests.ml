open Rstt
open Builder
open FunSig

(* Signatures are built explicitly in the first part of this file,
   and parsed (with [IO.parse_funsig]) in the second one. *)

let print name ty = Format.printf "%s: %a@." name Pp.ty (TyOp.simplify ty)
let print_sig name t = print name (to_regular t |> Builder.build TIdMap.empty)
let print_spec name t arg =
  match specialize t arg with
  | ty -> print name (Builder.build TIdMap.empty ty)
  | exception (Invalid_argument msg) -> Format.printf "%s: %s@." name msg

let env = ref Builder.empty_env
let resolve t = let env', t = FunSig.resolve !env t in env := env' ; t
let build t = let env', t = Builder.resolve !env t in env := env' ;
  Builder.build TIdMap.empty t

let absent = TOption TEmpty
let opt = FRegular absent
let str s = TVec (Vec.Scalar (PChr' s))
let int = TVec (Vec.Vector PSubInt)

(* ( a: { #k: 'a }, b = #k ) -> 'a *)
let get = resolve {
  dom = {
    pos_named = [
      LConst "a", FList { bindings=[LVar "k", FRegular (TVar "'a")] ; tl=opt } ;
      LConst "b", FLVar "k" ] ;
    pos_tl = opt ; named_tl = opt ; named = [] } ;
  ret = FRegular (TVar "'a") }

(* ( b = #k ) -> { #k: int } *)
let mk = resolve {
  dom = { pos_named = [ LConst "b", FLVar "k" ] ;
          pos_tl = opt ; named_tl = opt ; named = [] } ;
  ret = FList { bindings=[LVar "k", FRegular int] ; tl=opt } }

(* ( x: int ) -> int *)
let id = resolve {
  dom = { pos_named = [ LConst "x", FRegular int ] ;
          pos_tl = opt ; named_tl = opt ; named = [] } ;
  ret = FRegular int }

(* Call-site arguments *)
let call ?(named=[]) pos =
  build (TArg' { pos' = pos ; named' = named ; pos_tl' = absent ; named_tl' = absent })
let lst = TList { bindings=["foo", int] ; tl=absent }

let%expect_test "regular signatures" =
  Format.printf "%b %b@." (is_regular_ty get) (is_regular_ty id) ;
  print_sig "id" id ;
  begin match to_regular get with
  | _ -> Format.printf "no exception@."
  | exception (Invalid_argument msg) -> Format.printf "get: %s@." msg
  end ;
  [%expect {|
    false true
    id: (x: int) ->
    int
    get: Not a regular signature: label variable k is unresolved.
    |}]

let%expect_test "specialization" =
  print_spec "get" get (call [lst ; str "foo"]) ;
  print_spec "mk" mk (call [str "foo"]) ;
  (* The label variable can also be matched with a named argument *)
  print_spec "mk_named" mk (call ~named:["b", str "foo"] []) ;
  (* A union of strings yields an intersection of specialized signatures *)
  print_spec "mk_union" mk (call [TCup (str "foo", str "bar")]) ;
  (* Specializing a regular signature is the identity *)
  print_spec "id" id (call [int]) ;
  [%expect {|
    get: (a: { foo: 'a }, b: "foo") ->
    'a
    mk: (b: "foo") ->
    { foo: int }
    mk_named: (b: "foo") ->
    { foo: int }
    mk_union: ((b: "bar") -> { bar: int }) & ((b: "foo") ->
    { foo: int })
    id: (x: int) ->
    int
    |}]

let%expect_test "specialization failures" =
  (* The argument is not a string *)
  print_spec "not_a_string" mk (call [int]) ;
  (* The argument is not a scalar (a label must be a string of length 1) *)
  print_spec "not_a_scalar" mk (call [TVec (Vec.Vector PChr)]) ;
  (* The argument is missing *)
  print_spec "missing" mk (call []) ;
  (* The label variable is not matched with any parameter *)
  let unbound = resolve { dom = { pos_named = [] ; pos_tl = opt ; named_tl = opt ; named = [] } ;
                          ret = FList { bindings=[LVar "k", FRegular int] ; tl=opt } } in
  print_spec "unbound" unbound (call []) ;
  [%expect {|
    not_a_string: Cannot specialize the label variable k: it could not be resolved from the given argument.
    not_a_scalar: Cannot specialize the label variable k: it could not be resolved from the given argument.
    missing: Cannot specialize the label variable k: it could not be resolved from the given argument.
    unbound: Cannot specialize the label variable k: it could not be resolved from the given argument.
    |}]

let%expect_test "specialized signatures accept their argument" =
  let arg = call [TCup (str "foo", str "bar")] in
  let arrows = specialize mk arg |> Builder.build TIdMap.empty
    |> Attr.proj_content |> Ty.get_descr |> Descr.get_arrows in
  Format.printf "%b@." (Ty.leq arg (Op.Arrows.dom arrows)) ;
  print "res" (Op.Arrows.apply arrows arg) ;
  [%expect {|
    true
    res: { bar: int } |
    { foo: int }
    |}]

let%expect_test "unused label variables" =
  (* Every label variable must be resolved, even one that is never
     used as a label *)
  let unused = resolve { dom = { pos_named = [ LConst "b", FLVar "k" ] ;
                                 pos_tl = opt ; named_tl = opt ; named = [] } ;
                         ret = FRegular int } in
  print_spec "unused" unused (call [int]) ;
  print_spec "unused_resolved" unused (call [str "foo"]) ;
  [%expect {|
    unused: Cannot specialize the label variable k: it could not be resolved from the given argument.
    unused_resolved: (b: "foo") ->
    int
    |}]

(* ( x: { a: #k }, b = #k ) -> { #k: int } *)
let deep = resolve {
  dom = { pos_named = [
            LConst "x", FList { bindings=[LConst "a", FLVar "k"] ; tl=opt } ;
            LConst "b", FLVar "k" ] ;
          pos_tl = opt ; named_tl = opt ; named = [] } ;
  ret = FList { bindings=[LVar "k", FRegular int] ; tl=opt } }

(* ( x: any with { names: #k } ) -> { #k: int } *)
let attrs = resolve {
  dom = { pos_named = [
            LConst "x", FAttr { Attr.content = FRegular TAny ; classes = CAny ;
              attrs = FList { bindings=[LConst "names", FLVar "k"] ; tl=opt } } ] ;
          pos_tl = opt ; named_tl = opt ; named = [] } ;
  ret = FList { bindings=[LVar "k", FRegular int] ; tl=opt } }

let%expect_test "deep matching" =
  let lst b = TList { bindings=["a", b] ; tl=absent } in
  (* The label variable is resolved from a field of a list argument *)
  print_spec "deep" deep (call [lst (str "foo") ; str "foo"]) ;
  (* Constraints coming from several positions are intersected *)
  print_spec "deep_inter" deep (call [lst (str "foo") ; TCup (str "foo", str "bar")]) ;
  (* An unresolvable position is ignored as long as another one resolves it *)
  print_spec "deep_partial" deep (call [lst (TVec (Vec.Vector PChr)) ; str "foo"]) ;
  (* Contradictory constraints *)
  print_spec "deep_empty" deep (call [lst (str "foo") ; str "bar"]) ;
  (* The label variable is resolved from an attribute of the argument *)
  print_spec "attrs" attrs
    (call [TAttr { Attr.content=TAny ; classes=CAny ;
                   attrs=TList { bindings=["names", str "foo"] ; tl=absent } }]) ;
  [%expect {|
    deep: (x: { a: "foo" }, b: "foo") ->
    { foo: int }
    deep_inter: (x: { a: "foo" }, b: "foo") ->
    { foo: int }
    deep_partial: (x: { a: "foo" }, b: "foo") ->
    { foo: int }
    deep_empty: Cannot specialize the label variable k: no string can be matched with it.
    attrs: (x: any with { names: "foo" }) ->
    { foo: int }
    |}]

(* === Parsing === *)

let parse str =
  match Rstt_repl.IO.parse_funsig str with
  | t -> let env', t = FunSig.resolve !env t in env := env' ; Some t
  | exception (Rstt_repl.IO.SyntaxError (_, msg)) ->
    Format.printf "%s: %s@." str msg ; None

let print_parsed str =
  parse str |> Option.iter (fun t ->
    if is_regular_ty t then print_sig str t
    else Format.printf "%s: (not regular)@." str)

let%expect_test "parsing" =
  print_parsed "(x: int, y: lgl) -> CHR" ;
  print_parsed "(x: {a: int}) -> {b: lgl}" ;
  print_parsed "(x: int, ...: any, y: lgl) -> CHR" ;
  (* Types that FunSig cannot express are kept as regular types *)
  print_parsed "(x: {a: int} | {b: lgl}) -> [int, lgl]" ;
  (* Label variables, at any depth *)
  print_parsed "(a: {#k: 'a}, b = #k) -> 'a" ;
  print_parsed "(a: {#k: 'a}, b: #k) -> {#k: 'a}" ;
  print_parsed "(x: {a: #k}) -> {#k: int}" ;
  print_parsed "(x: any with {names: #k}) -> {#k: int}" ;
  [%expect {|
    (x: int, y: lgl) -> CHR: (x: int, y: lgl) ->
    CHR
    (x: {a: int}) -> {b: lgl}: (x: { a: int }) ->
    { b: lgl }
    (x: int, ...: any, y: lgl) -> CHR: (x: int, ...: any, y: lgl) ->
    CHR
    (x: {a: int} | {b: lgl}) -> [int, lgl]: (x: { a: int } | { b: lgl }) -> [int,
    lgl]
    (a: {#k: 'a}, b = #k) -> 'a: (not regular)
    (a: {#k: 'a}, b: #k) -> {#k: 'a}: (not regular)
    (x: {a: #k}) -> {#k: int}: (not regular)
    (x: any with {names: #k}) -> {#k: int}: (not regular)
    |}]

let%expect_test "parsing errors" =
  (* Not a single arrow *)
  print_parsed "int -> lgl" ;
  print_parsed "((x: int) -> lgl) & ((x: chr) -> chr)" ;
  print_parsed "(x: int)" ;
  (* Label variables in a position that FunSig cannot express *)
  print_parsed "(x: {#k: int} | list) -> lgl" ;
  print_parsed "(x: {#k: int}?) -> lgl" ;
  [%expect {|
    int -> lgl: syntax error
    ((x: int) -> lgl) & ((x: chr) -> chr): syntax error
    (x: int): syntax error
    (x: {#k: int} | list) -> lgl: Not a regular type: label variable k is unresolved.
    (x: {#k: int}?) -> lgl: Not a regular type: label variable k is unresolved.
    |}]

let%expect_test "parsing a regular type" =
  (* Label variables are rejected outside of a function signature *)
  let parse_ty str =
    match Rstt_repl.IO.parse_type str with
    | ty -> print str (build ty)
    | exception (Rstt_repl.IO.SyntaxError (_, msg)) -> Format.printf "%s: %s@." str msg
  in
  parse_ty "{a: int}" ;
  parse_ty "{#k: int}" ;
  parse_ty "(a: {#k: 'a}, b = #k) -> 'a" ;
  [%expect {|
    {a: int}: { a: int }
    {#k: int}: Not a regular type: label variable k is unresolved.
    (a: {#k: 'a}, b = #k) -> 'a: Not a regular argument: label variable k is unresolved.
    |}]

let%expect_test "parsing and specializing" =
  let t = parse "(a: {#k: 'a}, b = #k) -> 'a" |> Option.get in
  print_spec "get" t
    (call [TList { bindings=["foo", int] ; tl=absent } ; str "foo"]) ;
  let t = parse "(x: {a: #k}) -> {#k: int}" |> Option.get in
  print_spec "deep" t
    (call [TList { bindings=["a", str "foo"] ; tl=absent }]) ;
  [%expect {|
    get: (a: { foo: 'a }, b: "foo") ->
    'a
    deep: (x: { a: "foo" }) ->
    { foo: int }
    |}]
