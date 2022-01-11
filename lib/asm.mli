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

val pstring_of_integer : int -> program_string list

val pstring_of_string : string -> program_string list

val pstring_of_variable :
  string ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  program_string list

val pstring_of_pointer :
  Ast_types.address_value ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  program_string list

val pstring_of_subscript :
  Ast_types.address_value ->
  Ast_types.offset_value ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  program_string list

val string_of_static_value : Ast_types.static_value -> string

(* Statements *)

val pstring_of_macro_stmt : string -> program_string list

val pstring_of_if :
  scope:string ->
  expr:program_string list ->
  stmt_list:program_string list ->
  program_string list

val pstring_of_localvar : string -> program_string list -> program_string list

val pstring_of_assignment :
  program_string list -> program_string list -> program_string list

val pstring_of_subassignment :
  program_string list ->
  Ast_types.offset_value ->
  program_string list ->
  Ast_types.size_type ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  program_string list

(* Expressions *)

val pstring_of_eq :
  scope:string ->
  left_value:program_string list ->
  right_value:program_string list ->
  program_string list

(* Definitions *)

val pstring_of_near_funcdef :
  is_global:bool ->
  fname:string ->
  args:string list ->
  locals:string list ->
  stmt_list:program_string list ->
  program_string list

val pstring_of_staticvaruninitialized :
  is_global:bool ->
  stype:Ast_types.size_type ->
  sname:string ->
  program_string list

val pstring_of_staticvar :
  is_global:bool ->
  stype:Ast_types.size_type ->
  sname:string ->
  value:string ->
  program_string list

val pstring_of_extern : string list -> program_string list
