let usage_msg = Printf.sprintf "%s <file1> [-o <file2>]" Sys.argv.(0)

let compile = ref true

let input_file = ref [ "" ]

let output_file = ref "out.o"

let write_asm = ref false

let to_stdout = ref false

let set_write_asm () =
  write_asm := true;
  output_file := "out.asm"

let print_version () =
  compile := false;
  print_endline "r86 v0.1.0"

let anon_fun filename =
  match List.hd !input_file with
  | "" -> input_file := [ filename ]
  | _ ->
      Log.log @@ Log.SysError "More than one input file specified.";
      exit 1

let speclist =
  [
    ("-o", Arg.Set_string output_file, "Set output file name");
    ("-s", Arg.Unit set_write_asm, "Write assembly");
    ("-stdout", Arg.Set to_stdout, "Output assembly to stdout");
    ("-v", Arg.Unit print_version, "Print current version");
  ]
