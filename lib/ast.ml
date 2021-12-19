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

let rec eval_stmt_list scope stmt_list =
  let eval_stmt_scope = eval_stmt scope in
  List.map eval_stmt_scope stmt_list |> String.concat ""

and eval_stmt scope pstmt =
  match pstmt with
  | If (e, sl) ->
      let id = Random.int 10000 in
      let new_scope = Printf.sprintf "%s.if%d" scope id in
      Printf.sprintf
        "    ; IF<%d>\n\
        \    %s:\n\n\
        \    %s\n\
        \    cmp ax, 0\n\
        \    jnz %s.end\n\n\
         %s\n\n\
        \    %s.end:\n\n"
        id new_scope (eval_expr new_scope e) new_scope
        (eval_stmt_list new_scope sl)
        new_scope
  | MacroStmt m -> Printf.sprintf "    %s \n" m

and eval_expr scope pexpr =
  match pexpr with
  | Value v -> Printf.sprintf "mov ax, %s" (eval_value v)
  | Eq (lv, rv) ->
      let id = Random.int 10000 in
      let new_scope = Printf.sprintf "%s.eq%d" scope id in
      Printf.sprintf
        "; EQ<%d> %s %s\n\
        \    mov bx, %s\n\
        \    mov ax, %s\n\
        \    cmp bx, ax\n\
        \    jne %s.false\n\
        \    %s.true:\n\
        \    mov ax, 1\n\
        \    jmp %s.end\n\
        \    %s.false:\n\
        \    mov ax, 0\n\
        \    %s.end:\n"
        id (eval_value lv) (eval_value rv) (eval_value lv) (eval_value rv)
        new_scope new_scope new_scope new_scope new_scope

and eval_value = function Integer i -> string_of_int i | String s -> s

let rec eval_program defs_list =
  let string_of_pstring pstring =
    String.concat "\n"
      [
        pstring.header; pstring.text; pstring.data; pstring.rodata; pstring.bss;
      ]
  and concat_tree_string pstring_list =
    let concat (h, t, d, rd, b) pstring =
      (ignore
      @@ Buffer.
           ( add_string h pstring.header,
             add_string t pstring.text,
             add_string d pstring.data,
             add_string rd pstring.rodata,
             add_string b pstring.bss ));
      (h, t, d, rd, b)
    and unbuf (h, t, d, rd, b) =
      {
        header = Buffer.contents h;
        text = Buffer.contents t;
        data = Buffer.contents d;
        rodata = Buffer.contents rd;
        bss = Buffer.contents b;
      }
    in
    List.fold_left concat
      Buffer.(create 17, create 17, create 17, create 17, create 17)
      pstring_list
    |> unbuf
  in
  {
    header = Printf.sprintf "; %s \nBITS 16\n\n" Sys.argv.(1);
    text = "SECTION .text \n\n";
    data = "SECTION .data \n\n";
    rodata = "SECTION .rodata \n\n";
    bss = "SECTION .bss \n\n";
  }
  :: List.map eval_defs defs_list
  |> concat_tree_string |> string_of_pstring

and eval_defs = function
  | FuncDef (pt, lbl, args, sl) -> (
      match pt with
      | Near ->
          let func_string =
            Printf.sprintf
              "%s:\n\
              \    ; Near\n\
              \    ; Args: %s\n\
              \    push bp\n\
              \    mov bp, sp\n\n\
               %s\n\n\
              \    pop bp\n\
              \    ret\n"
              lbl (String.concat ", " args) (eval_stmt_list lbl sl)
          in
          { header = ""; text = func_string; data = ""; rodata = ""; bss = "" })
  | MacroDef m -> { header = m; text = ""; data = ""; rodata = ""; bss = "" }
