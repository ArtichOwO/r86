{
  open Parser

  let buf = Buffer.create 64
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
  | '"' { read_string lexbuf }
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
  | _ as c { raise @@ Exceptions.Invalid_character c }

and read_string = parse
  | '"' { let c = Buffer.contents buf in
          Buffer.clear buf; STRING c }
  | '\\' { Buffer.add_char buf @@ convert_char lexbuf ; read_string lexbuf }
  | [^ '"'] { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string lexbuf }
  | eof { raise Exceptions.String_never_terminated }
  | _ as c { raise @@ Exceptions.Invalid_character c }

and convert_char = parse
  | '"' { '"' }
  | '\\' { '\\' }
  | 'n' { '\n' }
  | 'r' { '\r' } 
  | 'b' { '\b' }
  | 't' { '\t' }
  | '0' { '\x00' }
