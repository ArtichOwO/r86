(* Values *)

val pstring_of_integer : int -> Pstring.t list

val pstring_of_string : string -> Pstring.t list

val pstring_of_variable :
  string ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  Pstring.t list

val pstring_of_pointer :
  Ast_types.address_value ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  Pstring.t list

val pstring_of_subscript :
  Ast_types.address_value ->
  Ast_types.offset_value ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  Pstring.t list

val string_of_static_value : Ast_types.static_value -> string

(* Statements *)

val pstring_of_macro_stmt : string -> Pstring.t list

val pstring_of_if :
  scope:string ->
  expr:Pstring.t list ->
  stmt_list:Pstring.t list ->
  Pstring.t list

val pstring_of_localvar : string -> Pstring.t list -> Pstring.t list

val pstring_of_assignment :
  Pstring.t list -> Pstring.t list -> Ast_types.size_type -> Pstring.t list

val pstring_of_subassignment :
  Pstring.t list ->
  Ast_types.offset_value ->
  Pstring.t list ->
  Ast_types.size_type ->
  Ast_types.arguments list ->
  Ast_types.arguments list ->
  Pstring.t list

(* Expressions *)

val pstring_of_eq :
  scope:string ->
  left_value:Pstring.t list ->
  right_value:Pstring.t list ->
  Pstring.t list

(* Definitions *)

val pstring_of_near_funcdef :
  is_global:bool ->
  fname:string ->
  args:string list ->
  locals:string list ->
  stmt_list:Pstring.t list ->
  Pstring.t list

val pstring_of_staticvaruninitialized :
  is_global:bool -> stype:Ast_types.size_type -> sname:string -> Pstring.t list

val pstring_of_staticvar :
  is_global:bool ->
  stype:Ast_types.size_type ->
  sname:string ->
  value:string ->
  Pstring.t list

val pstring_of_extern : string list -> Pstring.t list
