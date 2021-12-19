open Log

let get_line filename line_nb =
  let ic = open_in filename in
  for _ = 1 to line_nb - 1 do
    ignore (input_line ic)
  done;
  input_line ic

let parse lexbuf =
  try Ok (Parser.program Lexer.translate lexbuf) with
  | Lexer.Syntax_error (c, { lex_curr_p; _ }) ->
      Result.error
      @@ Error
           (Printf.sprintf "%d:%d:Invalid character `%c`" lex_curr_p.pos_lnum
              (lex_curr_p.pos_cnum - lex_curr_p.pos_bol)
              c)
  | Parser.Error ->
      let rec repeat_string s n =
        if n = 0 then "" else s ^ repeat_string s (n - 1)
      and char_offset =
        lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol
      in
      Result.error
      @@ Error
           (Printf.sprintf "%d:%d:Syntax error \n╮ %s  \n╰─%s╯"
              lexbuf.lex_curr_p.pos_lnum char_offset
              (get_line Sys.argv.(1) lexbuf.lex_curr_p.pos_lnum |> cyan)
              (char_offset - 1 |> repeat_string "─"))
