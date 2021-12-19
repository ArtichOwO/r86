let blue = Printf.sprintf "\027[0;34m%s\027[0m"

and red = Printf.sprintf "\027[0;31m%s\027[0m"

and orange = Printf.sprintf "\027[0;33m%s\027[0m"

let syntax_error msg = Printf.eprintf "Syntax error: %s\n" (red msg)
