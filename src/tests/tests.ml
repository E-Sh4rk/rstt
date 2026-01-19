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
    base: any
    base: vec
    tuple1: tuple0
    tuple2: true
    norm1: any
    norm2: clx | raw | dbl | int | chr | lgl
    norm3: clx | chr | lgl | raw | dbl
    vec1: raw42 | int1 | lgl['a] | lgl2
    vec2: true
    vec3: v['a]('b)
    lst1: true
    lst2: true
    lst3: false
    lst4: true
    lst5: { a: v1(^42) ... }
    lst6: { a: v1(^42) }
    lst7: { a: v1(^42) ; int }
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
            `R: { d : 'D | `R ;; `R }
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
    class5: <class1 ?>
    class6: int<class1 ?>
    c1: c(42) | c_string | c_na
    c2: c_true
    c3: c_int
    c4: ~c_na
    c5: *c_int
    |}]
