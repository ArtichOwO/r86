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

let string_of_pstring { header; text; data; rodata; bss } =
  String.concat "\n" [ header; text; data; rodata; bss ]

let pstring_headers =
  {
    header = Printf.sprintf "; %s \nBITS 16\n\n" Sys.argv.(1);
    text = "SECTION .text \n\n";
    data = "SECTION .data \n\n";
    rodata = "SECTION .rodata \n\n";
    bss = "SECTION .bss \n\n";
  }

let concat_tree_string pstring_list =
  let concat (h, t, d, rd, b) { header; text; data; rodata; bss } =
    (ignore
    @@ Buffer.
         ( add_string h header,
           add_string t text,
           add_string d data,
           add_string rd rodata,
           add_string b bss ));
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

(* Definitions *)

let pstring_of_near_funcdef ~is_global ~fname ~args ~locals ~stmt_list =
  let global =
    let header =
      if is_global then Printf.sprintf "GLOBAL %s \n" fname else ""
    in
    create_prgrm_string ~header ()
  and ptext_begin =
    let text =
      Printf.sprintf
        "%s:\n\
        \    ; Near\n\
        \    ; Args: %s\n\
        \    ; Locals: %s\n\
        \    push bp\n\
        \    mov bp, sp\n\n"
        fname (String.concat ", " args)
        (String.concat ", " locals)
    in
    create_prgrm_string ~text ()
  and ptext_end =
    let text = "\n\n    pop bp\n    ret\n" in
    create_prgrm_string ~text ()
  in
  [ global; ptext_begin ] @ stmt_list @ [ ptext_end ]

let pstring_of_staticvaruninitialized ~is_global ~stype ~sname =
  let global =
    let header =
      if is_global then Printf.sprintf "GLOBAL %s \n" sname else ""
    in
    create_prgrm_string ~header ()
  in
  let string_of_stype = match stype with Byte -> "resb" | Word -> "resw" in
  let svar_string = Printf.sprintf "%s %s 1\n" sname string_of_stype in
  [ global; create_prgrm_string ~bss:svar_string () ]

let pstring_of_staticvar ~is_global ~stype ~sname ~value =
  let global =
    let header =
      if is_global then Printf.sprintf "GLOBAL %s \n" sname else ""
    in
    create_prgrm_string ~header ()
  in
  let string_of_stype = match stype with Byte -> "db" | Word -> "dw" in
  let svar_string = Printf.sprintf "%s %s %s\n" sname string_of_stype value in
  [ global; create_prgrm_string ~data:svar_string () ]

let pstring_of_extern extern_list =
  let header =
    Printf.sprintf "EXTERN %s \n" @@ String.concat ", " extern_list
  in
  [ create_prgrm_string ~header () ]

(* Values *)

let string_of_static_value = function
  | StaticInteger i -> Printf.sprintf "0x%x" i
  | StaticString s ->
      let explode s = List.init (String.length s) (String.get s) in
      let convert_char c others =
        let new_char =
          match c with
          | '\x00' -> "0,"
          | '\x20' .. '\x7E' as c -> Printf.sprintf "\'%c\'," c
          | _ as c -> Char.code c |> Printf.sprintf "0x%x,"
        in
        new_char ^ others
      in
      let new_string = List.fold_right convert_char (explode s) "" in
      Printf.sprintf "%s0" new_string

let pstring_of_integer i =
  let text = Printf.sprintf "\n    mov ax, %d" i in
  [ create_prgrm_string ~text () ]

let pstring_of_string s =
  let id =
    let replace_char = function '-' -> '_' | _ as c -> c in
    Uuidm.v4_gen (Random.State.make_self_init ()) ()
    |> Uuidm.to_string ~upper:true
    |> String.map replace_char
  in
  let sname = Printf.sprintf "string_%s" id in
  let text = Printf.sprintf "\n    mov ax, %s" sname in
  let value = StaticString s |> string_of_static_value in
  [ create_prgrm_string ~text () ]
  @ pstring_of_staticvar ~is_global:false ~stype:Byte ~sname ~value

let pstring_of_variable var arg_list loc_list =
  let text =
    if List.mem_assoc var arg_list then
      let offset = List.assoc var arg_list in
      Printf.sprintf "\n    mov ax, ss\n    mov es, ax\n    mov ax, [bp+0x%x]"
        ((offset * 2) + 4)
    else if List.mem_assoc var loc_list then
      let offset = List.assoc var loc_list in
      Printf.sprintf "\n    mov ax, ss\n    mov es, ax\n    mov ax, [bp-0x%x]"
        ((offset * 2) + 2)
    else Printf.sprintf "\n    mov ax, ds\n    mov es, ax\n    mov ax, %s" var
  in
  [ create_prgrm_string ~text () ]

let pstring_of_pointer var arg_list loc_list =
  match var with
  | IntegerAddress (s, o) ->
      let text =
        Printf.sprintf "\n    mov ax, 0x%x\n    mov es, ax\n    mov ax, 0x%x" s
          o
      in
      [ create_prgrm_string ~text () ]
  | VariableAddress v ->
      let text =
        if List.mem_assoc v arg_list then
          let offset = List.assoc v arg_list in
          Printf.sprintf
            "\n    mov ax, ss\n    mov es, ax\n    mov ax, bp\n    add ax, 0x%x"
            ((offset * 2) + 4)
        else if List.mem_assoc v loc_list then
          let offset = List.assoc v loc_list in
          Printf.sprintf
            "\n    mov ax, ss\n    mov es, ax\n    mov ax, bp\n    sub ax, 0x%x"
            ((offset * 2) + 2)
        else Printf.sprintf "\n    mov ax, ds\n    mov es, ax\n    mov ax, %s" v
      in
      [ create_prgrm_string ~text () ]

let pstring_of_subscript addr offset arg_list loc_list =
  let str_ptr = pstring_of_pointer addr arg_list loc_list in
  match offset with
  | IntegerOffset i ->
      let ptext_begin =
        let text_begin = Printf.sprintf "\n    ; SUBSCRIPT" in
        create_prgrm_string ~text:text_begin ()
      and ptext_end =
        let text_end =
          Printf.sprintf
            "\n    mov si, ax\n    mov ax, [es:si+0x%x]\n    ; END SUBSCRIPT" i
        in
        create_prgrm_string ~text:text_end ()
      in
      [ ptext_begin ] @ str_ptr @ [ ptext_end ]
  | VariableOffset v ->
      let ptext_begin = create_prgrm_string ~text:"    ; SUBSCRIPT\n" ()
      and ptext_between = create_prgrm_string ~text:"\n    mov si, ax\n" ()
      and ptext_end =
        create_prgrm_string
          ~text:"\n    mov bx, ax\n    mov ax, [es:si+bx]\n    ; END SUBSCRIPT"
          ()
      in
      [ ptext_begin ] @ str_ptr @ [ ptext_between ]
      @ pstring_of_variable v arg_list loc_list
      @ [ ptext_end ]

(* Statements *)

let pstring_of_macro_stmt macro =
  let text = Printf.sprintf "%s \n" macro in
  [ create_prgrm_string ~text () ]

let pstring_of_if ~scope ~expr ~stmt_list =
  let id =
    let replace_char = function '-' -> '_' | _ as c -> c in
    Uuidm.v4_gen (Random.State.make_self_init ()) ()
    |> Uuidm.to_string ~upper:true
    |> String.map replace_char
  in
  let new_scope = Printf.sprintf "%s.if%s" scope id in

  let ptext_begin =
    let text_begin = Printf.sprintf "\n\n    ; IF<%s>\n    %s:" id new_scope in
    create_prgrm_string ~text:text_begin ()
  and ptext_between =
    let text_between =
      Printf.sprintf "\n    cmp ax, 0\n    jnz %s.end\n" new_scope
    in
    create_prgrm_string ~text:text_between ()
  and ptext_end =
    let text_end = Printf.sprintf "\n\n    %s.end:\n\n" new_scope in
    create_prgrm_string ~text:text_end ()
  in

  [ ptext_begin ] @ expr @ [ ptext_between ] @ stmt_list @ [ ptext_end ]

let pstring_of_localvar name value =
  let text = Printf.sprintf "\n    push ax ; LOCAL<%s>\n" name in
  value @ [ create_prgrm_string ~text () ]

let pstring_of_assignment address expr stype =
  let dest_reg = match stype with Byte -> "al" | Word -> "ax" in
  let addr_text = "\n    mov di, ax\n"
  and expr_text = Printf.sprintf "\n    mov [es:di], %s\n" dest_reg in
  address
  @ [ create_prgrm_string ~text:addr_text () ]
  @ expr
  @ [ create_prgrm_string ~text:expr_text () ]

let pstring_of_subassignment address offset expr stype arg_list loc_list =
  let dest_reg = match stype with Byte -> "al" | Word -> "ax"
  and ptext_begin = create_prgrm_string ~text:"\n    ; SUBSCRIPT ASSIGN" ()
  and ptext_between = create_prgrm_string ~text:"\n    mov si, ax" () in
  let ptext_end =
    match offset with
    | IntegerOffset i ->
        let text =
          Printf.sprintf
            "\n    mov [es:si+0x%x], %s\n    ; END SUBSCRIPT ASSIGN" i dest_reg
        in
        [ create_prgrm_string ~text () ]
    | VariableOffset v ->
        let text =
          Printf.sprintf
            "\n\
            \    mov bx, ax\n\
            \    mov [es:si+bx], %s\n\
            \    ; END SUBSCRIPT ASSIGN" dest_reg
        in
        pstring_of_variable v arg_list loc_list
        @ [ create_prgrm_string ~text () ]
  in
  [ ptext_begin ] @ address @ [ ptext_between ] @ expr @ ptext_end

(* Expressions *)

let pstring_of_eq ~scope ~left_value ~right_value =
  let id =
    let replace_char = function '-' -> '_' | _ as c -> c in
    Uuidm.v4_gen (Random.State.make_self_init ()) ()
    |> Uuidm.to_string ~upper:true
    |> String.map replace_char
  in
  let new_scope = Printf.sprintf "%s.eq%s" scope id in

  let ptext_begin =
    let text = Printf.sprintf "\n    ; EQ<%s>\n" id in
    create_prgrm_string ~text ()
  and ptext_left_value = create_prgrm_string ~text:"\n    mov bx, ax\n" ()
  and ptext_end =
    let text =
      Printf.sprintf
        "\n\
        \    cmp bx, ax\n\
        \    jne %s.false\n\
        \    %s.true:\n\
        \    mov ax, 1\n\
        \    jmp %s.end\n\
        \    %s.false:\n\
        \    mov ax, 0\n\
        \    %s.end:\n"
        new_scope new_scope new_scope new_scope new_scope
    in
    create_prgrm_string ~text ()
  in
  [ ptext_begin ] @ left_value @ [ ptext_left_value ] @ right_value
  @ [ ptext_end ]
