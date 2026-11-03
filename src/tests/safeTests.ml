open Rstt_repl

let run str =
  let buf = Lexing.from_string str in
  let rec aux env =
    match IO.parse_command buf with
    | End -> ()
    | Elt elt -> Repl.treat_elt env elt |> aux
  in
  Output.with_basic_output Format.std_formatter (fun () -> aux Repl.empty_env) ()

let run_safe str =
  Rstt.Config.safe_mode := true ;
  Fun.protect ~finally:(fun () -> Rstt.Config.safe_mode := false) (fun () -> run str)

let lossy = {|
"eptr": externalptr(INT, CHR) | externalptr(LGL, DBL) ;;
"cls": <`D> & <*> ;;
"arg": @(INT, ...: LGL) \ @(INT) ;;
|}

let exact = {|
"eptr": externalptr(INT) | externalptr(LGL) ;;
"cls": <. | `D> ;;
"arg": @(INT, LGL) | @(DBL, CHR) ;;
|}

let%expect_test "lossy printing" =
  run lossy ;
  [%expect {|
    eptr: externalptr(INT | LGL, CHR | DBL)
    cls: <`D>
    arg: @(INT, ...: LGL) | @(INT, ...: LGL) | @(INT, LGL?, ...: LGL)
    |}]

let%expect_test "safe mode on lossy printing" =
  run_safe lossy ;
  [%expect {|
    eptr: eptr({ _target : INT? ; _tag : CHR ..} | { _target : LGL? ; _tag : DBL
          ..})
    cls: class({  ;; tt & `D })
    arg: arg({ _id : empty? ; _npos : (1..) ; _pos : { _1 : INT ;; LGL? } ;
         _named : {  ;; LGL? } ..} \ { _id : empty? ; _npos : 1 ; _pos : { _1 :
         INT } ; _named : {  } ..})
    |}]

let%expect_test "safe mode on exact printing" =
  run_safe exact ;
  [%expect {|
    eptr: externalptr(INT | LGL)
    cls: <. | `D>
    arg: @(DBL, CHR) | @(INT, LGL)
    |}]

let%expect_test "set command" =
  Fun.protect ~finally:(fun () -> Rstt.Config.safe_mode := false) (fun () -> run {|
set safe_mode = true ;;
"safe": externalptr(INT, CHR) | externalptr(LGL, DBL) ;;
set safe_mode = false ;;
"unsafe": externalptr(INT, CHR) | externalptr(LGL, DBL) ;;
|}) ;
  [%expect {|
    safe: eptr({ _target : INT? ; _tag : CHR ..} | { _target : LGL? ; _tag : DBL
          ..})
    unsafe: externalptr(INT | LGL, CHR | DBL)
    |}] ;
  (try Rstt.Config.set "unknown" "true" with Invalid_argument msg -> print_endline msg) ;
  (try Rstt.Config.set "safe_mode" "maybe" with Invalid_argument msg -> print_endline msg) ;
  [%expect {|
    Unknown option unknown (available: safe_mode)
    Option safe_mode expects a boolean (true/false), got maybe
    |}]
