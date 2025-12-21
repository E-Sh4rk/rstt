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
    norm2: lgl | clx | raw | dbl | chr | int
    lst1: true
    lst2: true
    lst3: false
    lst4: true
    arg1: true
    arg2: true
    arg3: true
    arg4: true
    arg5: [
            'A: 'A \ prim
          ]
         [
           'A: 'A \ (chr | clx | dbl | lgl | raw) ;
           'B: 'B & (lgl) ;
           'C: 'C & (int) ;
           `R: { _npos : `R ; 0 : `R ; b : `R ; c : `R ; d : 'D | `R ;; empty? | `R }
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
    |}]
