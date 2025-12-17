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
    base: v(any)
    tuple1: tuple0
    tuple2: true
    norm1: any
    norm2: v(lgl) | v(clx) | v(raw) | v(dbl) | v(chr) | v(int)
    lst1: true
    lst2: true
    lst3: false
    lst4: true
    arg1: true
    arg2: true
    arg3: true
    arg4: true
    arg5: [
            'A: 'A \ any
          ]
         [
           'A: 'A \ (chr | clx | dbl | lgl | raw) ;
           'B: 'B & (v(lgl)) ;
           'C: 'C & (v(int)) ;
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
