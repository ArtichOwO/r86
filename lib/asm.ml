open Ast_types

type program_string = {
  header : string;
  text : string;
  data : string;
  rodata : string;
  bss : string;
}

(* PString *)

let create_prgrm_string ?(header = "") ?(text = "") ?(data = "") ?(rodata = "")
    ?(bss = "") () =
  { header; text; data; rodata; bss }

let string_of_pstring pstring =
  String.concat "\n"
    [ pstring.header; pstring.text; pstring.data; pstring.rodata; pstring.bss ]

let pstring_headers =
  {
    header = Printf.sprintf "; %s \nBITS 16\n\n" Sys.argv.(1);
    text = "SECTION .text \n\n";
    data = "SECTION .data \n\n";
    rodata = "SECTION .rodata \n\n";
    bss = "SECTION .bss \n\n";
  }

let concat_tree_string pstring_list =
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

(* Statements *)

let string_of_value_stmt = Printf.sprintf "mov ax, %s"

let string_of_macro_stmt = Printf.sprintf "%s \n"

let string_of_if ~scope ~expr ~stmt_list =
  let replace_char = function '-' -> '_' | _ as c -> c in
  let id =
    Uuidm.v4_gen (Random.State.make_self_init ()) ()
    |> Uuidm.to_string ~upper:true
    |> String.map replace_char
  in
  let new_scope = Printf.sprintf "%s.if%s" scope id in
  Printf.sprintf
    "    ; IF<%s>\n\
    \    %s:\n\n\
    \    %s\n\
    \    cmp ax, 0\n\
    \    jnz %s.end\n\n\
     %s\n\n\
    \    %s.end:\n\n"
    id new_scope expr new_scope stmt_list new_scope

(* Expressions *)

let string_of_eq ~scope ~left_value ~right_value =
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
    id left_value right_value left_value right_value new_scope new_scope
    new_scope new_scope new_scope

let string_of_variable_stmt var var_list =
  if List.mem_assoc var var_list then
    let offset = List.assoc var var_list in
    Printf.sprintf "mov ax, [bp+%d]" ((offset * 2) + 4)
  else Printf.sprintf "    mov ax, %s" var

(* Values *)

let string_of_variable_value var var_list =
  if List.mem_assoc var var_list then
    let offset = List.assoc var var_list in
    Printf.sprintf "[bp+%d]" ((offset * 2) + 4)
  else Printf.sprintf "%s" var

(* Definitions *)

let pstring_of_near_funcdef ~is_global ~fname ~args ~stmt_list =
  let ig = if is_global then Printf.sprintf "GLOBAL %s \n" fname else "" in
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
      fname (String.concat ", " args) stmt_list
  in
  create_prgrm_string ~header:ig ~text:func_string ()

let pstring_of_staticvaruninitialized ~is_global ~stype ~sname =
  let ig = if is_global then Printf.sprintf "GLOBAL %s \n" sname else "" in
  let string_of_stype = match stype with Byte -> "resb" | Word -> "resw" in
  let svar_string = Printf.sprintf "%s %s 1\n" sname string_of_stype in
  create_prgrm_string ~header:ig ~bss:svar_string ()

let pstring_of_staticvar ~is_global ~stype ~sname ~value =
  let ig = if is_global then Printf.sprintf "GLOBAL %s \n" sname else "" in
  let string_of_stype = match stype with Byte -> "db" | Word -> "dw" in
  let svar_string =
    Printf.sprintf "%s %s %s\n" sname string_of_stype value
  in
  create_prgrm_string ~header:ig ~data:svar_string ()

let pstring_of_extern extern_list =
  let extern_string =
    Printf.sprintf "EXTERN %s \n" (String.concat ", " extern_list)
  in
  create_prgrm_string ~header:extern_string ()
