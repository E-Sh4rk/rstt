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
    vec1: INT1 | LGL | raw1
    vec2: true
    vec3: vec1
    vec4: INT1('a)
    vec5: ^INT1('a)
    vec6: CHR1('a)
    vec7: ^CHR1('a)
    vec8: (INT1 -> c_true) & (vec & ~INT1 -> c_false)
    vec9: true
    vec10: true
    vec11: LGL | raw
    vec12: INT | LGL | raw
    vec13: CHR | INT | LGL | raw
    vec14: clx
    lst1: true
    lst2: true
    lst3: false
    lst4: true
    lst5: { a: 42, any }
    lst6: { a: ff }
    lst7: { a: "brrr", INT }
    lst8: { `r } -> { a: 42, `r }
    eptr1: externalptr
    eptr2: externalptr \ externalptr(empty)
    arg1: true
    arg2: true
    arg3: true
    arg4: true
    arg5: [
            'A: empty
          ]
          [
            'A: 'A & INT ;
            'B: 'B & LGL ;
            'C: 'C & INT ;
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
    arg6: ( a: INT, b: DBL ; any )
    arg7: @( INT, b: DBL ; any )
    arg8: ( a: INT, b: DBL ; INT ; named: raw)
    arg9: @( INT, b: DBL ; INT )
    arg10: true
    arg11: @( a: <class1> )
    arg12: @( INT?, LGL ; INT )
    arg13: @( LGL, INT? ; INT )
    arg14: ( a: INT ; absent, `r )
    arr1: INT --> INT
    arr2: INT -> INT
    arr3: INT -> INT
    arr4: INT -> INT
    class1: <class1>
    class2: <class1, `c>
    class3: [
              `C: { class1 : `C ; class2 : tt | `C ;; ff | `C }
            ]
    class4: <class1, class2>
    class5: <class1, ...>
    class6: INT<class1, ...>
    class7: <class1, ?class3>
    class8: <class1, ~class2, ...>
    class9: <~class2, ?class3, *>
    class10: <data.frame>
    attr1: INT with { dim: INT, any }
    attr2: INT with { dim: INT }
    attr3: INT with ({ dim: INT, any } & ~{ dim: INT })
    attr4: true
    attr5: true
    c1: c_int_na \ (c(..41) | c(43..)) | c_string \ c("abc")
    c2: c_true
    c3: c_int
    c4: ~(c_int_na \ c_int)
    c5: *c_int
    c6: c_int & c_int_na('a)
    c7: c_string('a)
    c8: c_int_na \ c(42)
    c9: c(..41) | c(43..)
    c10: c_null
    c11: *c_int \ c_null
    c12: *any
    c13: c_ptr
    c14: *any
    c15: c_ptr
    c16: c_int_na & c_int_na('a)
    c17: c_int_na \ c_int & c_int_na('a)
    sym: ( a: { #(b,2): 'a }, b: any? ) -> 'a
    |}]
