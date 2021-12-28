open Ast_types

val eval_program : defs list -> string

val eval_defs : defs -> Asm.program_string

val eval_stmt_list : string -> arguments list -> stmt list -> string

val eval_stmt : string -> arguments list -> stmt -> string

val eval_expr : string -> arguments list -> expr -> string

val eval_value : value -> arguments list -> string
