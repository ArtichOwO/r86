open Ast_types
open Asm_types

module Pointer = struct
  module Operand = struct
    let to_pstring ~args ~locals ~functype var =
      match var with
      | IntegerAddressOp i ->
          let text = [ Mov (Word, Register AX, OpInt i) ] in
          Pstring.create ~text ()
      | VariableAddressOp v ->
          let text =
            if List.mem_assoc v args then
              let offset = List.assoc v args in
              [
                Mov
                  ( Word,
                    Register AX,
                    MemnPos
                      ( Register BP,
                        OpInt
                          ((offset * 2)
                          + match functype with Near -> 4 | Far -> 6) ) );
              ]
            else if List.mem_assoc v locals then
              let offset = List.assoc v locals in
              [
                Mov
                  ( Word,
                    Register AX,
                    MemnNeg (Register BP, OpInt ((offset * 2) + 2)) );
              ]
            else [ Mov (Word, Register AX, OpLabel v) ]
          in
          Pstring.create ~text ()
  end

  let to_pstring ~args ~locals ~segment ~functype var =
    match var with
    | IntegerAddress (s, o) ->
        let text =
          [
            Mov (Word, Register AX, OpInt s);
            Mov (Word, Register ES, Register AX);
            Mov (Word, Register AX, OpInt o);
          ]
        in
        Pstring.create ~text ()
    | VariableAddress v ->
        let text =
          if List.mem_assoc v args then
            let offset = List.assoc v args in
            [
              Mov (Word, Register AX, Register SS);
              Mov (Word, Register ES, Register AX);
              Mov (Word, Register AX, Register BP);
              Add
                ( Register AX,
                  OpInt
                    ((offset * 2) + match functype with Near -> 4 | Far -> 6) );
            ]
          else if List.mem_assoc v locals then
            let offset = List.assoc v locals in
            [
              Mov (Word, Register AX, Register SS);
              Mov (Word, Register ES, Register AX);
              Mov (Word, Register AX, Register BP);
              Sub (Register AX, OpInt ((offset * 2) + 2));
            ]
          else
            [
              Mov (Word, Register AX, segment);
              Mov (Word, Register ES, Register AX);
              Mov (Word, Register AX, OpLabel v);
            ]
        in
        Pstring.create ~text ()
    | ComposedAddress (segment, address) ->
        let text = [ Mov (Word, Register ES, Register AX) ] in
        [
          Operand.to_pstring ~args ~locals ~functype segment;
          Pstring.create ~text ();
          Operand.to_pstring ~args ~locals ~functype address;
        ]
        |> Pstring.concat
end

module Variable = struct
  let to_pstring ~args ~locals var =
    let text =
      if List.mem_assoc var args then
        let offset = List.assoc var args in
        [
          Mov
            (Word, Register AX, MemnPos (Register BP, OpInt ((offset * 2) + 4)));
        ]
      else if List.mem_assoc var locals then
        let offset = List.assoc var locals in
        [
          Mov
            (Word, Register AX, MemnNeg (Register BP, OpInt ((offset * 2) + 2)));
        ]
      else [ Mov (Word, Register AX, Memn (OpLabel var)) ]
    in
    Pstring.create ~text ()
end

module Subscript = struct
  let to_pstring ~args ~locals ~functype stype address offset =
    let address_pstring =
      Pointer.to_pstring ~args ~locals ~functype ~segment:(Register DS) address
    and dst_reg =
      match stype with Byte -> Register AL | Word -> Register AX
    in
    match offset with
    | IntegerOffset i ->
        let offset = match stype with Byte -> i | Word -> i * 2 in
        let text_begin = [ Comment (true, "SUBSCRIPT") ]
        and text_end =
          [
            Mov (Word, Register SI, Register AX);
            (match stype with
            | Byte -> Xor (Register AH, Register AH)
            | Word -> Newline);
            Mov
              (stype, dst_reg, MemfPos (Register ES, Register SI, OpInt offset));
            Comment (true, "END SUBSCRIPT");
          ]
        in
        [
          Pstring.create ~text:text_begin ();
          address_pstring;
          Pstring.create ~text:text_end ();
        ]
        |> Pstring.concat
    | VariableOffset v ->
        let text_begin = [ Comment (true, "SUBSCRIPT") ]
        and text_between = [ Mov (Word, Register SI, Register AX) ]
        and text_end =
          [
              (match stype with
              | Byte -> Newline
              | Word -> Add (Register AX, Register AX));
              Mov (Word, Register BX, Register AX);
              (match stype with
              | Byte -> Xor (Register AH, Register AH)
              | Word -> Newline);
              Mov
                (stype, dst_reg, MemfPos (Register ES, Register SI, Register BX));
              Comment (true, "END SUBSCRIPT");
            ]
        in
        [
          Pstring.create ~text:text_begin ();
          address_pstring;
          Pstring.create ~text:text_between ();
          Variable.to_pstring v ~args ~locals;
          Pstring.create ~text:text_end ();
        ]
        |> Pstring.concat
end

let rec eval_stmt_list ~scope ~args ~locals ~functype stmt_list =
  let eval_stmt_scope = eval_stmt ~scope ~args ~locals ~functype in
  List.map eval_stmt_scope stmt_list |> List.flatten

and eval_stmt ~scope ~args ~locals ~functype pstmt =
  match pstmt with
  | If (e, sl) ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let new_scope = Printf.sprintf "%s.if%s" scope id in
      let text_begin =
        [
          Comment (true, Printf.sprintf "IF<%s>" id);
          LabelDef (true, new_scope);
          Newline;
        ]
      and text_between =
        [
          Newline;
          Cmp (Word, Register AX, OpInt 0);
          Jz (OpLabel (Printf.sprintf "%s.end" new_scope));
          Newline;
        ]
      and text_end =
        [ LabelDef (true, Printf.sprintf "%s.end" new_scope); Newline ]
      in

      [
        Pstring.create ~text:text_begin ();
        eval_expr ~scope ~args ~locals ~functype e;
        Pstring.create ~text:text_between ();
      ]
      @ eval_stmt_list ~scope ~args ~locals ~functype sl
      @ [ Pstring.create ~text:text_end () ]
  | IfElse (e, t, f) ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let new_scope = Printf.sprintf "%s.ifelse%s" scope id in

      let text_begin =
        [
          Comment (true, Printf.sprintf "IF ELSE<%s>" id);
          LabelDef (true, new_scope);
          Newline;
        ]
      and text_between =
        [
          Newline;
          Cmp (Word, Register AX, OpInt 0);
          Jz (OpLabel (Printf.sprintf "%s.false" new_scope));
          LabelDef (true, Printf.sprintf "%s.true" new_scope);
          Newline;
        ]
      and text_false =
        [
          Jmpn (OpLabel (Printf.sprintf "%s.end" new_scope));
          LabelDef (true, Printf.sprintf "%s.false" new_scope);
          Newline;
        ]
      and text_end =
        [ LabelDef (true, Printf.sprintf "%s.end" new_scope); Newline ]
      in
      [
        Pstring.create ~text:text_begin ();
        eval_expr ~scope ~args ~locals ~functype e;
        Pstring.create ~text:text_between ();
      ]
      @ eval_stmt_list ~scope ~args ~locals ~functype t
      @ [ Pstring.create ~text:text_false () ]
      @ eval_stmt_list ~scope ~args ~locals ~functype f
      @ [ Pstring.create ~text:text_end () ]
  | For (init, condition, inc, sl) ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let new_scope = Printf.sprintf "%s.for%s" scope id in

      [
        Pstring.create ~text:[ Comment (true, Printf.sprintf "FOR<%s>" id) ] ();
      ]
      @ (match init with
        | Some init_stmt -> eval_stmt ~scope ~args ~locals ~functype init_stmt
        | None -> [])
      @ [
          Pstring.create
            ~text:
              [ LabelDef (true, Printf.sprintf "%s.start" new_scope); Newline ]
            ();
        ]
      @ (match condition with
        | Some condition_expr ->
            [
              eval_expr ~scope ~args ~locals ~functype condition_expr;
              Pstring.create
                ~text:
                  [
                    Cmp (Word, Register AX, OpInt 0);
                    Jz (OpLabel (Printf.sprintf "%s.end" new_scope));
                    Newline;
                  ]
                ();
            ]
        | None -> [])
      @ eval_stmt_list ~scope ~args ~locals ~functype sl
      @ (match inc with
        | Some inc_stmt -> eval_stmt ~scope ~args ~locals ~functype inc_stmt
        | None -> [])
      @ [
          Pstring.create
            ~text:
              [
                Jmpn (OpLabel (Printf.sprintf "%s.start" new_scope));
                LabelDef (true, Printf.sprintf "%s.end" new_scope);
              ]
            ();
        ]
  | WhileUntil (condition, is_while, sl) ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let new_scope =
        if is_while then Printf.sprintf "%s.while%s" scope id
        else Printf.sprintf "%s.until%s" scope id
      in

      [
        Pstring.create
          ~text:
            [
              (if is_while then Comment (true, Printf.sprintf "WHILE<%s>" id)
              else Comment (true, Printf.sprintf "UNTIL<%s>" id));
              LabelDef (true, Printf.sprintf "%s.start" new_scope);
            ]
          ();
      ]
      @ [
          eval_expr ~scope ~args ~locals ~functype condition;
          Pstring.create
            ~text:
              [
                Cmp (Word, Register AX, OpInt 0);
                (if is_while then
                 Jz (OpLabel (Printf.sprintf "%s.end" new_scope))
                else Jnz (OpLabel (Printf.sprintf "%s.end" new_scope)));
                Newline;
              ]
            ();
        ]
      @ eval_stmt_list ~scope ~args ~locals ~functype sl
      @ [
          Pstring.create
            ~text:
              [
                Jmpn (OpLabel (Printf.sprintf "%s.start" new_scope));
                LabelDef (true, Printf.sprintf "%s.end" new_scope);
              ]
            ();
        ]
  | LocalVar (_, expr) ->
      let text = [ Push (Word, Register AX) ] in
      [ eval_expr ~scope ~args ~locals ~functype expr; Pstring.create ~text () ]
  | Assignment (stype, address, expr) ->
      let address_text = [ Mov (Word, Register DI, Register AX) ]
      and expr_text =
        [
          (match stype with
          | Byte -> Mov (Byte, Memf (Register ES, Register DI), Register AL)
          | Word -> Mov (Word, Memf (Register ES, Register DI), Register AX));
        ]
      in
      [
        Pointer.to_pstring address ~args ~locals ~functype
          ~segment:(Register DS);
        Pstring.create ~text:address_text ();
        eval_expr ~scope ~args ~locals ~functype expr;
        Pstring.create ~text:expr_text ();
      ]
  | SubAssignment (stype, address, offset, expr) ->
      let text_begin = [ Comment (true, "SUBSCRIPT ASSIGN") ]
      and text_expr =
        [
          (match stype with
          | Byte -> Mov (Byte, Register DL, Register AL)
          | Word -> Mov (Word, Register DX, Register AX));
        ]
      and text_between = [ Mov (Word, Register SI, Register AX) ]
      and ptext_end =
        match offset with
        | IntegerOffset i ->
            Pstring.create
              ~text:
                [
                  Mov
                    ( stype,
                      MemfPos (Register ES, Register SI, OpInt i),
                      match stype with
                      | Byte -> Register DL
                      | Word -> Register DX );
                  Comment (true, "END SUBSCRIPT ASSIGN");
                  Newline;
                ]
              ()
        | VariableOffset v ->
            [
              Variable.to_pstring v ~args ~locals;
              Pstring.create
                ~text:
                  [
                    Mov (Word, Register BX, Register AX);
                    Mov
                      ( stype,
                        MemfPos (Register ES, Register SI, Register BX),
                        match stype with
                        | Byte -> Register DL
                        | Word -> Register DX );
                    Comment (true, "END SUBSCRIPT ASSIGN");
                    Newline;
                  ]
                ();
            ]
            |> Pstring.concat
      in
      [
        Pstring.create ~text:text_begin ();
        eval_expr ~scope ~args ~locals ~functype expr;
        Pstring.create ~text:text_expr ();
        Pointer.to_pstring address ~args ~locals ~functype
          ~segment:(Register DS);
        Pstring.create ~text:text_between ();
        ptext_end;
      ]
  | FuncCall (func, el) ->
      let text_begin = [ Comment (true, "FUNC CALL") ]
      and eval_expr_push expr =
        [
          eval_expr ~scope ~args ~locals ~functype expr;
          Pstring.create ~text:[ Push (Word, Register AX) ] ();
        ]
        |> Pstring.concat
      in
      [ Pstring.create ~text:text_begin () ]
      @ List.map eval_expr_push el
      @ [
          Pointer.to_pstring ~args ~locals ~functype ~segment:(Register CS) func;
          Pstring.create
            ~text:
              [
                (match func with
                | VariableAddress _ -> Newline
                | _ -> Push (Word, Register ES));
                Calln (Register AX);
                Add (Register SP, OpInt (List.length el * 2));
              ]
            ();
        ]
  | InlineASM sl ->
      let instr_of_string t = Text t in
      let text =
        [ Comment (true, "INLINE ASM") ]
        @ List.map instr_of_string sl
        @ [ Newline ]
      in
      [ Pstring.create ~text () ]
  | Return e -> [ eval_expr ~scope ~args ~locals ~functype e ]

and eval_expr ~scope ~args ~locals ~functype pexpr : Pstring.t =
  match pexpr with
  | Value v -> eval_value ~args ~locals ~functype v
  | N_Eq (is_eq, lv, rv) ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let new_scope =
        if is_eq then Printf.sprintf "%s.eq%s" scope id
        else Printf.sprintf "%s.neq%s" scope id
      in
      let text_begin =
        [
          Comment
            ( true,
              if is_eq then Printf.sprintf "EQ<%s>" id
              else Printf.sprintf "NEQ<%s>" id );
        ]
      and text_left_value = [ Mov (Word, Register BX, Register AX) ]
      and text_end =
        [
          Cmp (Word, Register BX, Register AX);
          (if is_eq then Jne (OpLabel (Printf.sprintf "%s.false" new_scope))
          else Je (OpLabel (Printf.sprintf "%s.false" new_scope)));
          LabelDef (true, Printf.sprintf "%s.true" new_scope);
          Mov (Word, Register AX, OpInt 1);
          Jmpn (OpLabel (Printf.sprintf "%s.end" new_scope));
          LabelDef (true, Printf.sprintf "%s.false" new_scope);
          Mov (Word, Register AX, OpInt 0);
          LabelDef (true, Printf.sprintf "%s.end" new_scope);
        ]
      in
      [
        Pstring.create ~text:text_begin ();
        eval_value ~args ~locals ~functype lv;
        Pstring.create ~text:text_left_value ();
        eval_value ~args ~locals ~functype rv;
        Pstring.create ~text:text_end ();
      ]
      |> Pstring.concat
  | Operations opl ->
      let pstring_of_operation = function
        | OperationInt i ->
            Pstring.create
              ~text:
                [
                  Comment (true, Printf.sprintf "INT<%i>" i);
                  Push (Word, OpInt i);
                ]
              ()
        | OperationVar v ->
            [
              Pstring.create
                ~text:[ Comment (true, Printf.sprintf "VAR<%s>" v) ]
                ();
              Variable.to_pstring ~args ~locals v;
              Pstring.create ~text:[ Push (Word, Register AX) ] ();
            ]
            |> Pstring.concat
        | OperationAdd ->
            Pstring.create
              ~text:
                [
                  Comment (true, "ADD");
                  Pop (Word, Register BX);
                  Pop (Word, Register AX);
                  Add (Register AX, Register BX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationSub ->
            Pstring.create
              ~text:
                [
                  Comment (true, "SUB");
                  Pop (Word, Register BX);
                  Pop (Word, Register AX);
                  Sub (Register AX, Register BX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationMul ->
            Pstring.create
              ~text:
                [
                  Comment (true, "MUL");
                  Pop (Word, Register BX);
                  Pop (Word, Register AX);
                  Mul (Register BX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationDiv ->
            Pstring.create
              ~text:
                [
                  Comment (true, "DIV");
                  Pop (Word, Register CX);
                  Pop (Word, Register AX);
                  Div (Register CX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationMod ->
            Pstring.create
              ~text:
                [
                  Comment (true, "MOD");
                  Pop (Word, Register CX);
                  Pop (Word, Register AX);
                  Div (Register CX);
                  Push (Word, Register DX);
                ]
              ()
        | OperationAnd ->
            Pstring.create
              ~text:
                [
                  Comment (true, "AND");
                  Pop (Word, Register BX);
                  Pop (Word, Register AX);
                  And (Register AX, Register BX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationOr ->
            Pstring.create
              ~text:
                [
                  Comment (true, "OR");
                  Pop (Word, Register BX);
                  Pop (Word, Register AX);
                  Or (Register AX, Register BX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationXor ->
            Pstring.create
              ~text:
                [
                  Comment (true, "XOR");
                  Pop (Word, Register BX);
                  Pop (Word, Register AX);
                  Xor (Register AX, Register BX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationNot ->
            Pstring.create
              ~text:
                [
                  Comment (true, "NOT");
                  Pop (Word, Register AX);
                  Not (Register AX);
                  Push (Word, Register AX);
                ]
              ()
        | OperationSubscript (stype, address, offset) ->
            [
              Pstring.create ~text:[ Comment (true, "SUBSCRIPT") ] ();
              Subscript.to_pstring ~args ~locals ~functype stype address offset;
              Pstring.create ~text:[ Push (Word, Register AX) ] ();
            ]
            |> Pstring.concat
      in
      [ Pstring.create ~text:[ Comment (true, "OPERATIONS") ] () ]
      @ List.map pstring_of_operation opl
      @ [
          Pstring.create
            ~text:[ Comment (true, "RESULT"); Pop (Word, Register AX) ]
            ();
        ]
      |> Pstring.concat
  | FuncCallExpr (func, el) ->
      let text_begin = [ Comment (true, "FUNC CALL") ]
      and eval_expr_push expr =
        [
          eval_expr ~scope ~args ~locals ~functype expr;
          Pstring.create ~text:[ Push (Word, Register AX) ] ();
        ]
        |> Pstring.concat
      in
      [ Pstring.create ~text:text_begin () ]
      @ List.map eval_expr_push el
      @ [
          Pointer.to_pstring ~args ~locals ~functype ~segment:(Register CS) func;
          Pstring.create
            ~text:
              [
                (match func with
                | VariableAddress _ -> Newline
                | _ -> Push (Word, Register ES));
                Calln (Register AX);
                Add (Register SP, OpInt (List.length el * 2));
              ]
            ();
        ]
      |> Pstring.concat

and eval_value ~args ~locals ~functype value : Pstring.t =
  match value with
  | Integer i ->
      let text = [ Mov (Word, Register AX, OpInt i) ] in
      Pstring.create ~text ()
  | Variable var -> Variable.to_pstring var ~args ~locals
  | Subscript (stype, address, offset) ->
      Subscript.to_pstring ~args ~locals ~functype stype address offset
  | String s ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let sname = Printf.sprintf "string_%s" id in
      let text = [ Mov (Word, Register AX, OpLabel sname) ]
      and data = [ LabelDef (false, sname); Db (StaticString s) ] in
      Pstring.create ~text ~data ()

let rec eval_program defs_list =
  Pstring.headers :: (List.flatten @@ List.map eval_defs defs_list)
  |> Pstring.concat |> Pstring.to_string

and eval_defs = function
  | FuncDef { is_global; ftype; fname; args; stmt_list; locals } ->
      let rec create_arg_idx ?(index = 0) ?(args_list = []) ~args_string () =
        if List.length args_string = index then args_list
        else
          create_arg_idx ~index:(index + 1)
            ~args_list:(args_list @ [ (List.nth args_string index, index) ])
            ~args_string ()
      in
      let stmt_list_pstring =
        eval_stmt_list ~scope:fname
          ~args:(create_arg_idx ~args_string:args ())
          ~locals:(create_arg_idx ~args_string:locals ())
          ~functype:ftype stmt_list
        |> Pstring.concat
      in
      let header = if is_global then [ Global [ fname ] ] else []
      and text_begin =
        [
          LabelDef (false, fname);
          (match ftype with
          | Near -> Comment (true, "Near")
          | Far -> Comment (true, "Far"));
          Comment (true, Printf.sprintf "Args: %s" (String.concat "," args));
          Comment (true, Printf.sprintf "Locals: %s" (String.concat "," locals));
          Push (Word, Register BP);
          Mov (Word, Register BP, Register SP);
          Newline;
        ]
      and text_end =
        [
          Newline;
          Pop (Word, Register BP);
          (match ftype with Near -> Retn | Far -> Retf);
        ]
      in
      [
        Pstring.create ~header ~text:text_begin ();
        stmt_list_pstring;
        Pstring.create ~text:text_end ();
      ]
  | StaticVarUninitialized { is_global; stype; sname; size } ->
      let header = if is_global then [ Global [ sname ] ] else []
      and mnemo = match stype with Byte -> Resb size | Word -> Resw size in
      [ Pstring.create ~header ~bss:[ LabelDef (false, sname); mnemo ] () ]
  | StaticVar { is_global; stype; sname; value } ->
      let header = if is_global then [ Global [ sname ] ] else []
      and mnemo = match stype with Byte -> Db value | Word -> Dw value in
      [ Pstring.create ~header ~data:[ LabelDef (false, sname); mnemo ] () ]
  | StaticArray { is_global; stype; sname; values } ->
      let header = if is_global then [ Global [ sname ] ] else [] in
      let rec mnemo (d_values, d_strings) svalues =
        match svalues with
        | [] -> d_values @ d_strings
        | hd :: tl -> (
            match hd with
            | StaticString _ ->
                let id =
                  let replace_char = function '-' -> '_' | _ as c -> c in
                  Uuidm.v4_gen (Random.State.make_self_init ()) ()
                  |> Uuidm.to_string ~upper:true
                  |> String.map replace_char
                in
                let string_name = Printf.sprintf "string_%s" id in
                mnemo
                  ( d_values @ [ Dw (StaticLabel string_name) ],
                    d_strings @ [ LabelDef (false, string_name); Db hd ] )
                  tl
            | _ ->
                mnemo
                  ( d_values
                    @ [ (match stype with Byte -> Db hd | Word -> Dw hd) ],
                    d_strings )
                  tl)
      in
      [
        Pstring.create ~header
          ~data:([ LabelDef (false, sname) ] @ mnemo ([], []) values)
          ();
      ]
  | Extern extern_list ->
      let header = [ Extern extern_list ] in
      [ Pstring.create ~header () ]
