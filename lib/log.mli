val blue : string -> string

val red : string -> string

val yellow : string -> string

val magenta : string -> string

val cyan : string -> string

type msg =
  | Log of string
  | SysError of string
  | Error of string
  | Warning of string

val log : msg -> unit
