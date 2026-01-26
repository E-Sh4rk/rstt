{
  open Parser

  let enter_newline lexbuf =
    Lexing.new_line lexbuf;
    lexbuf

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c
}

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*
let varid = '\''['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*
let rvarid = '`'['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

let int = ('+'|'-')? ['0'-'9']+ ('_'+ ['0'-'9']+)*
let s = "vec" | "lgl" | "chr" | "int" | "dbl" | "clx" | "raw"
let vlen = 'v'['0'-'9']+
let slen = s ['0'-'9']+
let sbracket = s '['

rule token = parse
| "type" { TYPE }
| int as i { INT (Z.of_string i) }
| '"'      { read_string (Buffer.create 17) lexbuf }
| "v("  { VP } | "v[" { VB } | s as str { SHORT str } | "p("  { P } | "t(" { T } | "s(" { S } | "c(" { C }
| "int(" { PI } | "chr(" { PC } | "c_int(" { PCI }
| vlen as s { VLEN (String.sub s 1 ((String.length s) - 1) |> Z.of_string) }
| slen as s { SLEN (String.sub s 0 3, String.sub s 3 ((String.length s) - 3) |> Z.of_string) }
| sbracket as s { SBRACKET (String.sub s 0 ((String.length s) - 1)) }
| id as s  { ID s }
| varid as s  { VARID s }
| rvarid as s  { RVARID s }
| newline  { Lexing.new_line lexbuf ; token lexbuf }
| blank    { token lexbuf }
| ";;" { BREAK } | ',' { COMMA } | ':' { COLON } | ';' { SEMICOLON } | '=' { EQUAL }
| "..." { ELLIPSIS } | ".." { DPOINT } | '*' { STAR }
| "?" { QUESTION_MARK } | "^" { HAT } | "->" { ARROW } | "!" { EXCL_MARK }
| '(' { LPAREN } | ')' { RPAREN } | "{" { LBRACE } | "}" { RBRACE } | "@(" { ALPAREN }
| "[" { LBRACKET } | "]" { RBRACKET }
| "<=" { LEQ } | ">=" { GEQ } | "<" { LT } | ">" { GT }
| '|' { TOR } | '&' { TAND } | '~' { TNEG } | '\\' { TDIFF }
| eof { EOF }
| _ { raise (Errors.E_Lexer ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| newline { enter_newline lexbuf |> read_string buf }
| '"' { STRING (Buffer.contents buf) }
| '\\' (backslash_escapes as c) { Buffer.add_char buf (char_for_backslash c); read_string buf lexbuf }
| [^ '"' '\\' '\010' '\013']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf
  }
| _ { raise (Errors.E_Lexer ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (Errors.E_Lexer ("String is not terminated")) }
