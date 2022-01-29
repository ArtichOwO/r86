open R86
open Args

let () =
  Arg.parse speclist anon_fun usage_msg;
  match List.hd !input_file with
  | "" ->
      Log.log @@ Log.SysError "No input file specified.";
      exit 1
  | _ -> (
      try
        if !compile then
          if !write_asm then (
            let file_content = Core.In_channel.read_all @@ List.hd !input_file
            and oc = open_out !output_file in
            match Parsing.parse (Lexing.from_string file_content) with
            | Ok ast -> output_string oc @@ Ast.eval_program ast
            | Error msg ->
                Log.log msg;
                close_out oc)
          else if !to_stdout then
            let file_content =
              Core.In_channel.read_all @@ List.hd !input_file
            in
            match Parsing.parse (Lexing.from_string file_content) with
            | Ok ast -> print_endline @@ Ast.eval_program ast
            | Error msg -> Log.log msg
          else
            let _ = Sys.command "mkdir -p /tmp/r86"
            and file_content = Core.In_channel.read_all @@ List.hd !input_file
            and id =
              let replace_char = function '-' -> '_' | _ as c -> c in
              Uuidm.v4_gen (Random.State.make_self_init ()) ()
              |> Uuidm.to_string ~upper:true
              |> String.map replace_char
            in
            let oc = open_out @@ Printf.sprintf "/tmp/r86/%s.asm" id in
            match Parsing.parse (Lexing.from_string file_content) with
            | Ok ast ->
                output_string oc @@ Ast.eval_program ast;
                let status =
                  Unix.create_process "nasm"
                    [|
                      "nasm";
                      Printf.sprintf "/tmp/r86/%s.asm" id;
                      "-f";
                      "elf";
                      "-o";
                      !output_file;
                    |]
                    Unix.stdin Unix.stdout Unix.stderr
                in
                exit status
            | Error msg ->
                Log.log msg;
                close_out oc
      with e -> (
        match e with
        | Sys_error se -> Log.log @@ Log.SysError se
        | _ as error -> raise error))
