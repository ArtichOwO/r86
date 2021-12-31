let blue = Printf.sprintf "\027[0;34m%s\027[0m"

and red = Printf.sprintf "\027[0;31m%s\027[0m"

and yellow = Printf.sprintf "\027[0;33m%s\027[0m"

and magenta = Printf.sprintf "\027[0;35m%s\027[0m"

and cyan = Printf.sprintf "\027[0;36m%s\027[0m"

type msg =
  | Log of string
  | SysError of string
  | Error of string
  | Warning of string

let log = function
  | Log l -> Printf.printf "%s:%s \n" (yellow Sys.argv.(1)) l
  | SysError s ->
      Printf.eprintf "%s:%s:%s \n" (red "error") (yellow Sys.argv.(0)) s
  | Error e -> Printf.eprintf "%s:%s:%s \n" (red "error") (yellow Sys.argv.(1)) e
  | Warning w ->
      Printf.eprintf "%s:%s:%s \n" (yellow Sys.argv.(1)) (magenta "warn") w
