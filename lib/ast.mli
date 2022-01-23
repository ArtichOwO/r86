open Ast_types

val eval_program : defs list -> string

val eval_defs : defs -> Pstring.t list

val eval_stmt_list :
  string -> arguments list -> arguments list -> stmt list -> Pstring.t list

val eval_stmt :
  string -> arguments list -> arguments list -> stmt -> Pstring.t list

val eval_expr :
  string -> arguments list -> arguments list -> expr -> Pstring.t list

val eval_value : value -> arguments list -> arguments list -> Pstring.t list
