open Ast_types

val eval_program : defs list -> string

val eval_defs : defs -> Asm.program_string list

val eval_stmt_list :
  string ->
  arguments list ->
  arguments list ->
  stmt list ->
  Asm.program_string list

val eval_stmt :
  string -> arguments list -> arguments list -> stmt -> Asm.program_string list

val eval_expr :
  string -> arguments list -> arguments list -> expr -> Asm.program_string list

val eval_value :
  value -> arguments list -> arguments list -> Asm.program_string list
