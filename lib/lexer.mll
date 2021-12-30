{
  open Parser
  exception Syntax_error of char * Lexing.lexbuf
}

let newline = '\n' | "\r\n"

rule translate = parse
  | eof { EOF }
  | "%" [^ '\r' '\n']+ newline as m { Lexing.new_line lexbuf; MACRO m }
  | "/*" _* "*/" { translate lexbuf }
  | "//" [^ '\r' '\n']* (newline) { Lexing.new_line lexbuf; translate lexbuf }
  | newline { Lexing.new_line lexbuf; translate lexbuf }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '=' { ASSIGN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '*' { ASTERISK }
  | "for" { FOR }
  | "if" { IF }
  | "else" { ELSE }
  | "near" { NEAR }
  | "far" { FAR }
  | "int" { INT }
  | "global" { GLOBAL }
  | "extern" { EXTERN }
  | "byte" { BYTE }
  | "word" { WORD }
  | "==" { EQ }
  | "!=" { NEQ }
  | ['0'-'9']+ as i { INTEGER (int_of_string i) }
  | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ as i { INTEGER (int_of_string i) }
  | ['0'-'9' 'a'-'z' 'A'-'Z' '_']+ as str { LABEL str }
  | [' ' '\t'] { translate lexbuf }
  | _ as c { raise (Syntax_error (c,lexbuf)) }
