(* Lexing errors *)

exception Syntax_error of char * Lexing.lexbuf

(* Parsing errors *)

exception Pointer_overflow
