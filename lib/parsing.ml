open Log

let get_line filename line_nb =
  let ic = open_in filename in
  for _ = 1 to line_nb - 1 do
    ignore (input_line ic)
  done;
  input_line ic

let error_msg ~msg ~line ~linenb ~colnb =
  let rec repeat_string s n =
    if n = 0 then "" else s ^ repeat_string s (n - 1)
  in
  Error
    (Printf.sprintf "%d:%d:%s \n╮ %s  \n╰─%s╯" linenb colnb msg (line |> cyan)
       (colnb - 1 |> repeat_string "─"))

let parse lexbuf =
  try Ok (Parser.program Lexer.translate lexbuf) with
  | Exceptions.Syntax_error (c, { lex_curr_p; _ }) ->
      Result.error
      @@ error_msg
           ~msg:(Printf.sprintf "Invalid character `%c`" c)
           ~line:(get_line Sys.argv.(1) lexbuf.lex_curr_p.pos_lnum)
           ~linenb:lex_curr_p.pos_lnum
           ~colnb:(lex_curr_p.pos_cnum - lex_curr_p.pos_bol)
  | Parser.Error ->
      let colnb = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      Result.error
      @@ error_msg ~msg:"Syntax error"
           ~line:(get_line Sys.argv.(1) lexbuf.lex_curr_p.pos_lnum)
           ~linenb:lexbuf.lex_curr_p.pos_lnum ~colnb
  | Exceptions.Pointer_overflow ->
      let colnb = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      Result.error
      @@ error_msg ~msg:"Pointer value overflow"
           ~line:(get_line Sys.argv.(1) lexbuf.lex_curr_p.pos_lnum)
           ~linenb:lexbuf.lex_curr_p.pos_lnum ~colnb
