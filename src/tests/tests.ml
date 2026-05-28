open Rstt_repl

let%expect_test "tests" =
  let fn = "tests.txt" in
  let cin = open_in fn in
  let buf = Lexing.from_channel cin in
  let rec test env =
    match IO.parse_command buf with
    | End -> ()
    | Elt elt ->
      let env = Repl.treat_elt env elt in
      (*[%expect {| |}] ; *) test env
  in
  Output.with_basic_output Format.std_formatter
    (fun () -> test Repl.empty_env) () ;
  [%expect {|
    base1: any
    base2: vec
    base3: <...>
    base4: ~<...>
    tuple1: tuple0
    tuple2: true
    norm1: any
    norm2: chr | clx | dbl | int | lgl | raw
    norm3: chr | clx | dbl | lgl | raw
    vec1: int1 | lgl2 | lgl['a] | raw42
    vec2: true
    vec3: v['a]('b)
    vec4: v(int(i(int & 'a)))
    vec5: v(chr(s(enum & 'a)))
    vec6: ((int1 -> c_true) & (int[^(int \ 1)] | vec & ~int -> c_false))<...>
    vec7: true
    lst1: true
    lst2: true
    lst3: false
    lst4: true
    lst5: { a: 42, any }
    lst6: { a: ff }
    lst7: { a: "brrr", int }
    lst8: ({ `r } -> { a: 42, `r })<...>
    eptr1: externalptr<...>
    eptr2: (externalptr \ externalptr(empty))<...>
    arg1: true
    arg2: true
    arg3: true
    arg4: true
    arg5: [
            'A: 'A \ prim
          ]
          [
            'A: 'A \ p(chr | clx | dbl | lgl | raw) ;
            'B: 'B & lgl ;
            'C: 'C & int ;
            `R: { b : `R ; c : `R ; d : 'D | `R ;; empty? | `R }
          ]
          [
            'B: empty
          ]
          [
            'C: empty
          ]
          [
            'D: empty
          ]
    arg6: ( a: int, b: dbl ; any )
    arg7: @( int, b: dbl ; any )
    arg8: ( a: int, b: dbl ; int ; named: raw)
    arg9: @( int, b: dbl ; int )
    arg10: true
    arg11: @( a: <class1> )
    arg12: @( int?, lgl ; int )
    arg13: @( lgl, int? ; int )
    arg14: ( a: int ; absent, `r )
    class1: <class1>
    class2: <class1, `c>
    class3: [
              `C: { class1 : `C ; class2 : tt | `C ;; ff | `C }
            ]
    class4: <class1, class2>
    class5: <class1, ...>
    class6: int<class1, ...>
    class7: <class1, ?class3>
    class8: <class1, ~class2, ...>
    class9: <~class2, ?class3, *>
    c1: (c_na | c(42)) | c_string \ c("abc")
    c2: c_true
    c3: c_int
    c4: ~c_na
    c5: *c_int
    c6: cint(int & 'a)
    c7: cstring(enum & 'a) \ c_null
    c8: c_int_na \ c(42)
    c9: c_int \ c(42)
    c10: c_null
    c11: *c_int \ c_null
    c12: *any \ c_null
    c13: c_ptr
    c14: *any \ c_null
    c15: c_ptr
    sym: (( a: { #(b,2): 'a }, b: any? ) -> 'a)<...>
    |}]
