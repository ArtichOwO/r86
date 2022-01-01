(* Lexing errors *)

exception Syntax_error of char

(* Parsing errors *)

exception Pointer_overflow

exception Integer_overflow

exception String_as_words
