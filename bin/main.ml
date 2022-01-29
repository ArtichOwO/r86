open R86

let () =
  if Array.length Sys.argv == 2 then
    let file_content = Core.In_channel.read_all Sys.argv.(1) in
    match Parsing.parse (Lexing.from_string file_content) with
    | Ok ast -> Ast.eval_program ast |> print_endline
    | Error msg -> Log.log msg
  else Log.log @@ Log.SysError "No filename specified or too much arguments";
  exit 1
