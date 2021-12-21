type program_string = {
  header : string;
  text : string;
  data : string;
  rodata : string;
  bss : string;
}

and program = defs list

and defs =
  | FuncDef of {
      is_global : unit option;
      ftype : function_type;
      fname : string;
      args : string list;
      stmt_list : stmt list;
    }
  | MacroDef of string
  | StaticVarUninitialized of {
      is_global : unit option;
      stype : static_type;
      sname : string;
    }
  | StaticVar of {
      is_global : unit option;
      stype : static_type;
      sname : string;
      value : value;
    }
  | Extern of string list

and label = string

and stmt = If of expr * stmt list | MacroStmt of string

and expr = Value of value | Eq of value * value | Variable of string

and function_type = Near

and static_type = Byte | Word

and value = Integer of int | String of string

and arguments = string * int

val eval_program : defs list -> string

val eval_defs : defs -> program_string

val eval_stmt_list : string -> arguments list -> stmt list -> string

val eval_stmt : string -> arguments list -> stmt -> string

val eval_expr : string -> arguments list -> expr -> string

val eval_value : value -> string
