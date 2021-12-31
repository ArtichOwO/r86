type program_string

(* PString *)

val create_prgrm_string :
  ?header:string ->
  ?text:string ->
  ?data:string ->
  ?rodata:string ->
  ?bss:string ->
  unit ->
  program_string

val string_of_pstring : program_string -> string

val pstring_headers : program_string

val concat_tree_string : program_string list -> program_string

(* Values *)

val string_of_integer : int -> string

val string_of_variable : string -> Ast_types.arguments list -> string

val string_of_pointer :
  Ast_types.address_value -> Ast_types.arguments list -> string

val string_of_subscript :
  Ast_types.address_value ->
  Ast_types.offset_value ->
  Ast_types.arguments list ->
  string

val string_of_static_value : Ast_types.static_value -> string

(* Statements *)

val string_of_macro_stmt : string -> string

val string_of_if : scope:string -> expr:string -> stmt_list:string -> string

(* Expressions *)

val string_of_eq :
  scope:string -> left_value:string -> right_value:string -> string

(* Definitions *)

val pstring_of_near_funcdef :
  is_global:bool ->
  fname:string ->
  args:string list ->
  stmt_list:string ->
  program_string

val pstring_of_staticvaruninitialized :
  is_global:bool ->
  stype:Ast_types.static_type ->
  sname:string ->
  program_string

val pstring_of_staticvar :
  is_global:bool ->
  stype:Ast_types.static_type ->
  sname:string ->
  value:string ->
  program_string

val pstring_of_extern : string list -> program_string
