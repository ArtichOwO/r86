type program_string = {
  header : string;
  text : string;
  data : string;
  rodata : string;
  bss : string;
}

and program = defs list

and defs =
  | FuncDef of function_type * string * string list * stmt list
  | MacroDef of string

and label = string

and stmt = If of expr * stmt list | MacroStmt of string

and expr = Value of value | Eq of value * value

and function_type = Near

and value = Integer of int | String of string

val eval_program : defs list -> string

val eval_defs : defs -> program_string

val eval_stmt_list : string -> stmt list -> string

val eval_stmt : string -> stmt -> string

val eval_expr : string -> expr -> string

val eval_value : value -> string
