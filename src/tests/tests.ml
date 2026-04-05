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
    tuple1: tuple0
    tuple2: true
    norm1: any
    norm2: chr | raw | clx | lgl | dbl | int
    norm3: chr | dbl | clx | raw | lgl
    vec1: raw42 | lgl2 | int1 | lgl['a]
    vec2: true
    vec3: v['a]('b)
    vec4: v(int(i('a & int)))
    vec5: v(chr(s('a & enum)))
    vec6: (int[^(int \ 1)] | vec & ~int -> c_false) & (int1 -> c_true)
    vec7: true
    lst1: true
    lst2: true
    lst3: false
    lst4: true
    lst5: { a: 42 ... }
    lst6: { a: ff }
    lst7: { a: "brrr" ; int }
    lst8: {  ; `r } -> { a: 42 ; `r }
    arg1: true
    arg2: true
    arg3: true
    arg4: true
    arg5: [
            'A: 'A \ prim
          ]
          [
            'A: 'A \ (chr | clx | dbl | lgl | raw) ;
            'B: 'B & lgl ;
            'C: 'C & int ;
            `R: { _id : `R ; _npos : `R ; 0 : `R ; b : `R ; c : `R ; d : 'D | `R
            ;; empty? | `R }
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
    arg6: ( a: int, b: dbl ... )
    arg7: @( int, b: dbl ... )
    arg8: ( a: int, b: dbl ; int ; named: raw)
    arg9: @( int, b: dbl ; int )
    arg10: true
    arg11: @( a: <class1> )
    class1: <class1>
    class2: <class1 ; `c>
    class3: [
              `C: { class1 : `C ; class2 : tt | `C ;; ff | `C }
            ]
    class4: <(class1, class2)>
    class5: <class1 ...>
    class6: int<class1 ...>
    c1: (c(42) | c_na) | c_string \ c("abc")
    c2: c_true
    c3: c_int
    c4: ~c_na
    c5: *c_int
    c6: cint('a & int)
    c7: cstring('a & enum)
    c8: c_int_na \ c(42)
    c9: c_int \ c(42)
    |}]
