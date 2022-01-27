open Ast_types
open Asm_types

module Pointer = struct
  module Operand = struct
    let to_pstring ~args ~locals var =
      match var with
      | IntegerAddressOp i ->
          let text = [ Mov (Word, Register AX, OpInt i) ] in
          Pstring.create ~text ()
      | VariableAddressOp v ->
          let text =
            if List.mem_assoc v args then
              let offset = List.assoc v args in
              [
                Mov (Word, Register AX, Register BP);
                Add (Register AX, OpInt ((offset * 2) + 4));
              ]
            else if List.mem_assoc v locals then
              let offset = List.assoc v locals in
              [
                Mov (Word, Register AX, Register BP);
                Sub (Register AX, OpInt ((offset * 2) + 2));
              ]
            else [ Mov (Word, Register AX, OpLabel v) ]
          in
          Pstring.create ~text ()
  end

  let to_pstring ~args ~locals var =
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
              Add (Register AX, OpInt ((offset * 2) + 4));
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
              Mov (Word, Register AX, Register DS);
              Mov (Word, Register ES, Register AX);
              Mov (Word, Register AX, OpLabel v);
            ]
        in
        Pstring.create ~text ()
    | ComposedAddress (segment, address) ->
        let text = [ Mov (Word, Register ES, Register AX) ] in
        [
          Operand.to_pstring ~args ~locals segment;
          Pstring.create ~text ();
          Operand.to_pstring ~args ~locals address;
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

let rec eval_stmt_list ~scope ~args ~locals stmt_list =
  let eval_stmt_scope = eval_stmt ~scope ~args ~locals in
  List.map eval_stmt_scope stmt_list |> List.flatten

and eval_stmt ~scope ~args ~locals pstmt =
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
          Jnz (OpLabel (Printf.sprintf "%s.end" new_scope));
          Newline;
        ]
      and text_end =
        [ LabelDef (true, Printf.sprintf "%s.end" new_scope); Newline ]
      in

      [
        Pstring.create ~text:text_begin ();
        eval_expr ~scope ~args ~locals e;
        Pstring.create ~text:text_between ();
      ]
      @ eval_stmt_list ~scope ~args ~locals sl
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
        | Some init_stmt -> eval_stmt ~scope ~args ~locals init_stmt
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
              eval_expr ~scope ~args ~locals condition_expr;
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
      @ eval_stmt_list ~scope ~args ~locals sl
      @ (match inc with
        | Some inc_stmt -> eval_stmt ~scope ~args ~locals inc_stmt
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
  | LocalVar (_, value) ->
      let text = [ Push (Word, Register AX) ] in
      [ eval_value ~args ~locals value; Pstring.create ~text () ]
  | Assignment (address, expr, stype) ->
      let dest_reg =
        match stype with Byte -> Register AL | Word -> Register AX
      in
      let address_text = [ Mov (Word, Register DI, Register AX) ]
      and expr_text =
        [ Mov (stype, Memf (Register ES, Register DI), dest_reg) ]
      in
      [
        Pointer.to_pstring address ~args ~locals;
        Pstring.create ~text:address_text ();
        eval_expr ~scope ~args ~locals expr;
        Pstring.create ~text:expr_text ();
      ]
  | SubAssignment (address, offset, expr, stype) ->
      let dest_reg =
        match stype with Byte -> Register AL | Word -> Register AX
      in
      let text_begin = [ Comment (true, "SUBSCRIPT ASSIGN") ]
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
                      dest_reg );
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
                        dest_reg );
                    Comment (true, "END SUBSCRIPT ASSIGN");
                    Newline;
                  ]
                ();
            ]
            |> Pstring.concat
      in
      [
        Pstring.create ~text:text_begin ();
        Pointer.to_pstring address ~args ~locals;
        Pstring.create ~text:text_between ();
        eval_expr ~scope ~args ~locals expr;
        ptext_end;
      ]

and eval_expr ~scope ~args ~locals pexpr : Pstring.t =
  match pexpr with
  | Value v -> eval_value ~args ~locals v
  | Eq (lv, rv) ->
      let id =
        let replace_char = function '-' -> '_' | _ as c -> c in
        Uuidm.v4_gen (Random.State.make_self_init ()) ()
        |> Uuidm.to_string ~upper:true
        |> String.map replace_char
      in
      let new_scope = Printf.sprintf "%s.eq%s" scope id in
      let text_begin = [ Comment (true, Printf.sprintf "EQ<%s>" id) ]
      and text_left_value = [ Mov (Word, Register BX, Register AX) ]
      and text_end =
        [
          Cmp (Word, Register BX, Register AX);
          Jne (OpLabel (Printf.sprintf "%s.false" new_scope));
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
        eval_value ~args ~locals lv;
        Pstring.create ~text:text_left_value ();
        eval_value ~args ~locals rv;
        Pstring.create ~text:text_end ();
      ]
      |> Pstring.concat

and eval_value ~args ~locals value : Pstring.t =
  match value with
  | Integer i ->
      let text = [ Mov (Word, Register AX, OpInt i) ] in
      Pstring.create ~text ()
  | Variable var -> Variable.to_pstring var ~args ~locals
  | Subscript (address, offset) -> (
      let address_pstring = Pointer.to_pstring ~args ~locals address in
      match offset with
      | IntegerOffset i ->
          let text_begin = [ Comment (true, "SUBSCRIPT") ]
          and text_end =
            [
              Mov (Word, Register SI, Register AX);
              Mov
                (Word, Register AX, MemfPos (Register ES, Register SI, OpInt i));
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
              Mov (Word, Register BX, Register AX);
              Mov
                ( Word,
                  Register AX,
                  MemfPos (Register ES, Register SI, Register BX) );
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
          |> Pstring.concat)
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
  | FuncDef { is_global; ftype; fname; args; stmt_list; locals } -> (
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
          stmt_list
        |> Pstring.concat
      in
      match ftype with
      | Near ->
          let header = if is_global then [ Global [ fname ] ] else []
          and text_begin =
            [
              LabelDef (false, fname);
              Comment (true, "Near");
              Comment (true, Printf.sprintf "Args: %s" (String.concat "," args));
              Comment
                (true, Printf.sprintf "Locals: %s" (String.concat "," locals));
              Push (Word, Register BP);
              Mov (Word, Register BP, Register SP);
              Newline;
            ]
          and text_end = [ Newline; Pop (Word, Register BP); Retn ] in
          [
            Pstring.create ~header ~text:text_begin ();
            stmt_list_pstring;
            Pstring.create ~text:text_end ();
          ])
  | StaticVarUninitialized { is_global; stype; sname } ->
      let header = if is_global then [ Global [ sname ] ] else []
      and mnemo = match stype with Byte -> Resb 1 | Word -> Resw 1 in
      [ Pstring.create ~header ~bss:[ LabelDef (false, sname); mnemo ] () ]
  | StaticVar { is_global; stype; sname; value } ->
      let header = if is_global then [ Global [ sname ] ] else []
      and mnemo = match stype with Byte -> Db value | Word -> Dw value in
      [ Pstring.create ~header ~data:[ LabelDef (false, sname); mnemo ] () ]
  | Extern extern_list ->
      let header = [ Extern extern_list ] in
      [ Pstring.create ~header () ]
