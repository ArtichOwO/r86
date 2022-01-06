(* Lexing errors *)

exception Invalid_character of char

exception String_never_terminated

(* Parsing errors *)

exception Pointer_overflow

exception Integer_overflow

exception String_as_words
