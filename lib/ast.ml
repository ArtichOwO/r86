open Ast_types

let rec eval_stmt_list scope var_list stmt_list =
  let eval_stmt_scope = eval_stmt scope var_list in
  List.map eval_stmt_scope stmt_list |> String.concat ""

and eval_stmt scope var_list pstmt =
  match pstmt with
  | If (e, sl) ->
      Asm.string_of_if ~scope
        ~expr:(eval_expr scope var_list e)
        ~stmt_list:(eval_stmt_list scope var_list sl)
  | MacroStmt macro -> Asm.string_of_macro_stmt macro

and eval_expr scope var_list pexpr =
  match pexpr with
  | Value v -> eval_value v var_list
  | Eq (lv, rv) ->
      Asm.string_of_eq ~scope ~left_value:(eval_value lv var_list)
        ~right_value:(eval_value rv var_list)

and eval_value value var_list =
  match value with
  | Integer i -> Asm.string_of_integer i
  | Variable var -> Asm.string_of_variable var var_list
  | Subscript (addr, offset) -> Asm.string_of_subscript addr offset var_list

let rec eval_program defs_list =
  Asm.pstring_headers :: List.map eval_defs defs_list
  |> Asm.concat_tree_string |> Asm.string_of_pstring

and eval_defs = function
  | FuncDef { is_global; ftype; fname; args; stmt_list } -> (
      let rec create_arg_idx ~args_list ~index =
        if List.length args = index then args_list
        else
          create_arg_idx
            ~args_list:(args_list @ [ (List.nth args index, index) ])
            ~index:(index + 1)
      in
      let stmt_list_string =
        eval_stmt_list fname (create_arg_idx ~args_list:[] ~index:0) stmt_list
      in
      match ftype with
      | Near ->
          Asm.pstring_of_near_funcdef ~is_global ~stmt_list:stmt_list_string
            ~fname ~args)
  | MacroDef m -> Asm.create_prgrm_string ~header:m ()
  | StaticVarUninitialized { is_global; stype; sname } ->
      Asm.pstring_of_staticvaruninitialized ~is_global ~stype ~sname
  | StaticVar { is_global; stype; sname; value } ->
      Asm.pstring_of_staticvar ~is_global ~stype ~sname
        ~value:(Asm.string_of_static_value value)
  | Extern extern_list -> Asm.pstring_of_extern extern_list
