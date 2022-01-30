type program = defs list

and defs =
  | FuncDef of {
      is_global : bool;
      ftype : function_type;
      fname : string;
      args : string list;
      stmt_list : stmt list;
      locals : string list;
    }
  | StaticVarUninitialized of {
      is_global : bool;
      stype : size_type;
      sname : string;
      size : int;
    }
  | StaticVar of {
      is_global : bool;
      stype : size_type;
      sname : string;
      value : static_value;
    }
  | StaticArray of {
      is_global : bool;
      stype : size_type;
      sname : string;
      values : static_value list;
    }
  | Extern of string list

and label = string

and stmt =
  | If of expr * stmt list
  | IfElse of expr * stmt list * stmt list
  | For of stmt option * expr option * stmt option * stmt list
  | WhileUntil of expr * bool * stmt list
  | LocalVar of string * expr
  | Assignment of address_value * expr * size_type
  | SubAssignment of address_value * offset_value * expr * size_type
  | FuncCall of address_value * expr list
  | InlineASM of string list
  | Return of expr

and expr =
  | Value of value
  | N_Eq of bool * size_type * value * value
  | Operations of operation list
  | FuncCallExpr of address_value * expr list

and function_type = Near | Far

and size_type = Byte | Word

and value =
  | Integer of int
  | Variable of string
  | Subscript of size_type * address_value * offset_value
  | String of string

and offset_value = IntegerOffset of int | VariableOffset of string

and address_value =
  | IntegerAddress of int * int
  | VariableAddress of string
  | ComposedAddress of address_operand * address_operand

and address_operand = IntegerAddressOp of int | VariableAddressOp of string

and static_value =
  | StaticInteger of int
  | StaticString of string
  | StaticLabel of string

and arguments = string * int

and operation =
  | OperationInt of int
  | OperationVar of string
  | OperationSubscript of size_type * address_value * offset_value
  | OperationAdd
  | OperationSub
  | OperationMul
  | OperationDiv
  | OperationMod
  | OperationAnd
  | OperationOr
  | OperationXor
  | OperationNot
