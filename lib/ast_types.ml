type program = defs list

and defs =
  | FuncDef of {
      is_global : bool;
      ftype : function_type;
      fname : string;
      args : string list;
      stmt_list : stmt list;
    }
  | MacroDef of string
  | StaticVarUninitialized of {
      is_global : bool;
      stype : static_type;
      sname : string;
    }
  | StaticVar of {
      is_global : bool;
      stype : static_type;
      sname : string;
      value : value;
    }
  | Extern of string list

and label = string

and stmt = If of expr * stmt list | MacroStmt of string

and expr =
  | Value of value
  | Eq of value * value
  | VariableExpr of string
  | SubscriptExpr of value * address_value

and function_type = Near

and static_type = Byte | Word

and value = Integer of int | String of string | VariableValue of string

and address_value = IntegerAddress of int | VariableAddress of string

and arguments = string * int
