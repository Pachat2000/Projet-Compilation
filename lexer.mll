{
  open Lexing
  open Parser

  exception Error of char
}

let alpha = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let identifier = alpha (alpha | num | '-' | '_' )*

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| num+ as n       { Lint (int_of_string n) }
| "true"          { Lbool true }
| "false"         { Lbool false }
| ';'             { Lsc }
| '"'             {  read_string (Buffer.create 17) lexbuf }
| '*'             { Lmul }
| "while"         { Lwhile }
| '+'             { Ladd }
| "&&"            { Land }
| '-'             { Lsub }
| '|'             { Labs }
| '!'             { Lnot }
| '\\'            { Ldiv }
| "if"            { Lif  }
| "else"          { Lelse }
| "puti"          { Lputi }
| "geti"          { Lgeti }
| "puts"          { Lputs }
| '('             { Lparo }
| ')'             { Lparf }
| '{'             { Lcro }
| '}'             { Lcrf }
| ','             { Lvrg }
| "str"           { Lstr }
| "return"        { Lreturn }
| "int"           { Lintd }
| "bool"          { Lboold }
| "void"          { Lvoid }
| '='             { Leq }
| "!="            { Lsne }
| "=="            { Leqb }
| ">"             { Lsgt }
| ">="            { Lsge }
| "<"             { Lslt }
| "<="            { Lsle }
| identifier as n { Lvar n }
| _ as c          { raise (Error c) }

and read_string buf =
  parse
  | '"'       { Lgui (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }