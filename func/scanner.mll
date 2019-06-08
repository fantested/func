(* Ocamllex scanner for FunC *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let letter = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']

rule token = parse
(* Whitespace *)
  [' ' '\t' '\r' '\n'] { token lexbuf }

(* Comments *)
| "/*"     { comment lexbuf }

(* Enclosures *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }

(* Delimiters *)
| ';'      { SEMI }
| ','      { COMMA }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MODULO }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "=>"     { ANON }
| "|>"     { PIPE }
| "|"      { BAR }

(* Control flow *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Types *)
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "func"   { FUNC }

(* Built-in functions *)
(* | "len"    { LEN } *)

(* Malloc *)
| "new"    { NEW }

(* Literals *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { ILIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| '"' ((ascii | escape)* as lxm) '"' { SLIT(lxm) }
| letter (letter | digit | '_')* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
