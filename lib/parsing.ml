open Log
open Exceptions

type line_info = { line : string; linenb : int; colnb : int }

let get_line filename line_nb =
  let ic = open_in filename in
  for _ = 1 to line_nb - 1 do
    ignore (input_line ic)
  done;
  input_line ic

let error_msg ~msg { line; linenb; colnb } =
  let rec repeat_string s n =
    if n = 0 then "" else s ^ repeat_string s (n - 1)
  in
  Error
    (Printf.sprintf "%d:%d:%s \n╮ %s  \n╰─%s╯" linenb colnb msg (line |> cyan)
       (colnb - 1 |> repeat_string "─"))

let create_line_info ~filename ~pos_lnum ~pos_cnum ~pos_bol =
  {
    line = get_line filename pos_lnum;
    linenb = pos_lnum;
    colnb = pos_cnum - pos_bol;
  }

let parse lexbuf =
  let filename = Sys.argv.(1) in
  try Ok (Parser.program Lexer.translate lexbuf)
  with _ as error ->
    Result.error
    @@ error_msg
         ~msg:
           (match error with
           | Invalid_character c -> Printf.sprintf "Invalid character `%c`" c
           | String_never_terminated -> "String never terminated"
           | Asm_never_starts -> "Inline assembly never starts"
           | Asm_never_terminated -> "Inline assembly never terminated"
           | Parser.Error -> "Syntax error"
           | Pointer_overflow -> "Pointer value overflow"
           | Integer_overflow -> "Integer overflow"
           | String_as_words -> "String can only be defined as bytes"
           | Label_redefinition s -> Printf.sprintf "Label \"%s\" redefined" s
           | Label_not_defined s -> Printf.sprintf "Label \"%s\" not defined" s
           | _ as other -> raise other)
    @@ create_line_info ~filename ~pos_lnum:lexbuf.lex_curr_p.pos_lnum
         ~pos_cnum:lexbuf.lex_curr_p.pos_cnum ~pos_bol:lexbuf.lex_curr_p.pos_bol
