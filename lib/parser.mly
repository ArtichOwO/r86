%{
  open Ast_types

  type func_item = {
    name: string;
    args: string list;
    locals: string BatDynArray.t;
  }

  type func_decl = {
    is_global : bool;
    ftype : function_type;
    fname : string;
    args : string list;
  }

  let func_list : func_item BatDynArray.t = BatDynArray.of_list [ 
    { 
      name = "";
      args = [];
      locals = BatDynArray.create ();
    } 
  ]

  let is_top_name_redef label =
    let is_same_name func = 
      func.name = label
    in BatDynArray.exists is_same_name func_list

  let is_loc_name_redef label = 
    let current_func = BatDynArray.last func_list in
    is_top_name_redef label 
    || BatDynArray.mem label current_func.locals 
    || List.mem label current_func.args
%}

%token SEMICOLON
%token COLON
%token COMMA
%token EOF

%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK

%token ASTERISK
%token PLUS
%token HYPHEN
%token SLASH
%token PERCENT
%token AND
%token VERTICAL
%token CIRCUMFLEX
%token TILDE

%token LET
%token FOR
%token WHILE
%token UNTIL
%token IF ELSE
%token RETURN

%token TRUE FALSE
%token NULL

%token <string list> ASM

%token ASSIGN
%token EQ NEQ
%token NEAR FAR INT
%token BYTE WORD

%token GLOBAL EXTERN

%token <string> STRING
%token <string> LABEL
%token <int> INTEGER

%start <program> program

%%

program: e=defs*; EOF { e }

defs:
  | fd=func_decl;LBRACE;stmt_list=stmt*;RBRACE 
    { let current = BatDynArray.last func_list in
      let { is_global; ftype; fname; args } = fd 
      and locals = BatDynArray.to_list current.locals in
      FuncDef { is_global; ftype; fname; args; stmt_list; locals } }
  | d=defs_wo_semicolon;SEMICOLON { d }

defs_wo_semicolon:
  | ig=option(GLOBAL);stype=size_type;sname=LABEL
    { if is_top_name_redef sname 
      then raise @@ Exceptions.Label_redefinition sname
      else BatDynArray.add func_list { name=sname; args=[]; locals=(BatDynArray.create ())};
      let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
      StaticVarUninitialized { is_global; stype; sname; size = 1 } }
  | ig=option(GLOBAL);stype=size_type;sname=LABEL;LBRACK;i=INTEGER;RBRACK
    { if is_top_name_redef sname 
      then raise @@ Exceptions.Label_redefinition sname
      else BatDynArray.add func_list { name=sname; args=[]; locals=(BatDynArray.create ())};
      let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
      StaticVarUninitialized { is_global; stype; sname; size = i } }
  | ig=option(GLOBAL);stype=size_type;sname=LABEL;ASSIGN;value=static_value
    { if is_top_name_redef sname 
      then raise @@ Exceptions.Label_redefinition sname
      else BatDynArray.add func_list { name=sname; args=[]; locals=(BatDynArray.create ())};
      begin match value with
      | StaticInteger i ->
        (match stype with
        | Byte -> if i > 0xFF then raise Exceptions.Integer_overflow
        | Word -> if i > 0xFFFF then raise Exceptions.Integer_overflow)
      | StaticString _ -> 
        (match stype with
        | Word -> raise Exceptions.String_as_words
        | _ -> ())
      | StaticLabel _ -> ()
      end;
      let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
      StaticVar { is_global; stype; sname; value } }
  | ig=option(GLOBAL);stype=size_type;sname=LABEL;ASSIGN;LBRACE;values=array_item+;RBRACE
    { if is_top_name_redef sname 
      then raise @@ Exceptions.Label_redefinition sname
      else BatDynArray.add func_list { name=sname; args=[]; locals=(BatDynArray.create ())};
      let rec check_if_correct lst =
        match lst with
        | [] -> ()
        | hd::tl -> (match hd with
          | StaticInteger i ->
            (match stype with
            | Byte -> if i > 0xFF then raise Exceptions.Integer_overflow
            | Word -> if i > 0xFFFF then raise Exceptions.Integer_overflow)
          | StaticString _ -> 
            (match stype with
            | Byte -> raise Exceptions.Pointer_overflow
            | _ -> ())
          | StaticLabel _ -> ()); check_if_correct tl;
      in check_if_correct values;
      let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
      StaticArray { is_global; stype; sname; values } }
  | EXTERN;externl=argument+ 
    { let is_same_name_map label =
        if is_top_name_redef label 
        then raise @@ Exceptions.Label_redefinition label
        else BatDynArray.add func_list { name=label; args=[]; locals=(BatDynArray.create ())};
      in List.iter is_same_name_map externl;
      Extern externl }

function_type:
  | NEAR { Near }
  | FAR { Far }
  | INT { Int }

size_type:
  | BYTE { Byte }
  | WORD { Word }

argument: lbl=LABEL;option(COMMA) { lbl }

func_decl: ig=option(GLOBAL);ftype=function_type;fname=LABEL;LPAREN;args=argument*;RPAREN
  { if is_top_name_redef fname 
    then raise @@ Exceptions.Label_redefinition fname;
    let is_same_name label =
      if is_loc_name_redef label
      then raise @@ Exceptions.Label_redefinition label
    in List.iter is_same_name args;
    BatDynArray.add func_list { name=fname; args; locals=(BatDynArray.create ())};
    let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
    { is_global; ftype; fname; args } }

stmt:
  | IF;i=expr;LBRACE;t=stmt*;RBRACE { If (i,t) }
  | IF;i=expr;LBRACE;t=stmt*;RBRACE;ELSE;LBRACE;f=stmt*;RBRACE { IfElse (i,t,f) }
  | FOR;LPAREN;init=option(stmt_wo_semicolon);SEMICOLON;
    condition=option(expr);SEMICOLON;
    inc=option(stmt_wo_semicolon);RPAREN;LBRACE;sl=stmt*;RBRACE 
    { For (init, condition, inc, sl) }
  | WHILE;condition=expr;LBRACE;sl=stmt*;RBRACE { WhileUntil (condition, true, sl) }
  | UNTIL;condition=expr;LBRACE;sl=stmt*;RBRACE { WhileUntil (condition, false, sl) }
  | s=stmt_wo_semicolon;SEMICOLON { s }

stmt_wo_semicolon:
  | LET;l=LABEL
    { let current_func = BatDynArray.last func_list in
      if is_loc_name_redef l 
      then raise @@ Exceptions.Label_redefinition l
      else BatDynArray.add current_func.locals l;
      LocalVar (l, Value (Integer 0)) }
  | LET;l=LABEL;ASSIGN;v=expr 
    { let current_func = BatDynArray.last func_list in
      if is_loc_name_redef l 
      then raise @@ Exceptions.Label_redefinition l
      else BatDynArray.add current_func.locals l;
      LocalVar (l, v) }
  | st=option(size_type);l=address_value;ASSIGN;e=expr 
    { let stype = match st with
        | None -> Word
        | Some stype -> stype
      in
      Assignment (stype,l,e) }
  | st=option(size_type);l=address_value;LBRACK;o=offset_value;RBRACK;ASSIGN;e=expr
    { let stype = match st with
        | None -> Word
        | Some stype -> stype
      in
      SubAssignment (stype,l,o,e) }
  | st=option(size_type);ASTERISK;l=address_value;ASSIGN;e=expr
    { let stype = match st with
        | None -> Word
        | Some stype -> stype
      in
      SubAssignment (stype,l,(IntegerOffset 0),e) }
  | func=address_value;LPAREN;el=funccall_argument*;RPAREN
    { FuncCall (func, el) }
  | sl=ASM { InlineASM sl }
  | RETURN;e=expr { Return e }

expr:
  | LPAREN;lv=value;EQ;rv=value;RPAREN 
    { N_Eq (true,lv,rv) }
  | LPAREN;lv=value;NEQ;rv=value;RPAREN 
    { N_Eq (false,lv,rv) }
  | v=value { Value v }
  | LPAREN;v=value;RPAREN { Value v }
  | LPAREN;opl=operation+;RPAREN { Operations opl }
  | f=funccall { f }
  
funccall:
  | func=address_value;LPAREN;el=funccall_argument*;RPAREN
    { FuncCallExpr (func, el) }

funccall_argument:
  | e=expr;option(COMMA) { e }

value:
  | i=INTEGER { Integer i }
  | s=STRING+ { String (String.concat "" s) }
  | v=LABEL 
    { if not @@ is_loc_name_redef v 
      then raise @@ Exceptions.Label_not_defined v;
      Variable v }
  | st=option(size_type);address=address_value;LBRACK;offset=offset_value;RBRACK 
    { let stype = match st with
      | None -> Word
      | Some stype -> stype 
      in
      Subscript (stype,address,offset) }
  | st=option(size_type);ASTERISK;address=address_value 
    { let stype = match st with
      | None -> Word
      | Some stype -> stype 
      in 
      Subscript (stype,address,(IntegerOffset 0)) }
  | TRUE { Integer 1 }
  | FALSE { Integer 0 }
  | NULL { Integer 0 }

offset_value:
  | i=INTEGER { IntegerOffset i }
  | v=LABEL { VariableOffset v }

address_value:
  | i=INTEGER { if i > 0xFFFFF then raise Exceptions.Pointer_overflow;
                IntegerAddress (((Int.shift_right i 16) * 0x1000),(Int.logand i 0xFFFF)) }
  | v=LABEL 
    { if not @@ is_loc_name_redef v 
      then raise @@ Exceptions.Label_not_defined v;
      VariableAddress v }
  | segment=address_operand;COLON;address=address_operand 
    { ComposedAddress (segment, address) }

address_operand:
  | i=INTEGER { if i > 0xFFFF then raise Exceptions.Pointer_overflow;
                IntegerAddressOp i }
  | v=LABEL
    { if not @@ is_loc_name_redef v 
      then raise @@ Exceptions.Label_not_defined v;
      VariableAddressOp v }

static_value:
  | i=INTEGER { StaticInteger i }
  | s=STRING+ { StaticString (String.concat "" s) }

array_item:
  | v=static_value;option(COMMA) { v }

operation:
  | e=expr 
    { match e with 
      | Value v -> 
        (match v with
         | String _ -> raise @@ Exceptions.String_in_operation
         | _ -> OperationExpr e)
      | _ -> OperationExpr e }
  | PLUS { OperationAdd }
  | ASTERISK { OperationMul }
  | HYPHEN { OperationSub }
  | SLASH { OperationDiv }
  | PERCENT { OperationMod }
  | AND { OperationAnd }
  | VERTICAL { OperationOr }
  | CIRCUMFLEX { OperationXor }
  | TILDE { OperationNot }
