open Ast_types

let rec eval_stmt_list scope arg_list loc_list stmt_list =
  let eval_stmt_scope = eval_stmt scope arg_list loc_list in
  List.map eval_stmt_scope stmt_list |> List.flatten

and eval_stmt scope arg_list loc_list pstmt =
  match pstmt with
  | If (e, sl) ->
      Asm.pstring_of_if ~scope
        ~expr:(eval_expr scope arg_list loc_list e)
        ~stmt_list:(eval_stmt_list scope arg_list loc_list sl)
  | MacroStmt macro -> Asm.pstring_of_macro_stmt macro
  | LocalVar (name, value) ->
      eval_value value arg_list loc_list |> Asm.pstring_of_localvar name
  | Assignment (addr, expr) ->
      eval_expr scope arg_list loc_list expr
      |> Asm.pstring_of_assignment
         @@ Asm.pstring_of_pointer addr arg_list loc_list
  | SubAssignment (addr, offset, expr, size_type) ->
      Asm.pstring_of_subassignment
        (Asm.pstring_of_pointer addr arg_list loc_list)
        offset
        (eval_expr scope arg_list loc_list expr)
        size_type arg_list loc_list

and eval_expr scope arg_list loc_list pexpr =
  match pexpr with
  | Value v -> eval_value v arg_list loc_list
  | Eq (lv, rv) ->
      Asm.pstring_of_eq ~scope
        ~left_value:(eval_value lv arg_list loc_list)
        ~right_value:(eval_value rv arg_list loc_list)

and eval_value value arg_list loc_list =
  match value with
  | Integer i -> Asm.pstring_of_integer i
  | Variable var -> Asm.pstring_of_variable var arg_list loc_list
  | Subscript (addr, offset) ->
      Asm.pstring_of_subscript addr offset arg_list loc_list
  | String s -> Asm.pstring_of_string s

let rec eval_program defs_list =
  Asm.pstring_headers :: (List.flatten @@ List.map eval_defs defs_list)
  |> Asm.concat_tree_string |> Asm.string_of_pstring

and eval_defs = function
  | FuncDef { is_global; ftype; fname; args; stmt_list; locals } -> (
      let rec create_arg_idx ?(index = 0) ?(args_list = []) ~args_string () =
        if List.length args_string = index then args_list
        else
          create_arg_idx ~index:(index + 1)
            ~args_list:(args_list @ [ (List.nth args_string index, index) ])
            ~args_string ()
      in
      let stmt_list_pstring =
        eval_stmt_list fname
          (create_arg_idx ~args_string:args ())
          (create_arg_idx ~args_string:locals ())
          stmt_list
      in
      match ftype with
      | Near ->
          Asm.pstring_of_near_funcdef ~is_global ~stmt_list:stmt_list_pstring
            ~fname ~args ~locals)
  | MacroDef m -> [ Asm.create_prgrm_string ~header:m () ]
  | StaticVarUninitialized { is_global; stype; sname } ->
      Asm.pstring_of_staticvaruninitialized ~is_global ~stype ~sname
  | StaticVar { is_global; stype; sname; value } ->
      Asm.pstring_of_staticvar ~is_global ~stype ~sname
        ~value:(Asm.string_of_static_value value)
  | Extern extern_list -> Asm.pstring_of_extern extern_list
