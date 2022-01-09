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

  let func_list : func_item BatDynArray.t = BatDynArray.create ()
%}

%token SEMICOLON
%token COMMA
%token EOF

%token <string> MACRO

%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK

%token ASTERISK

%token LET
%token FOR
%token IF ELSE

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
  | m=MACRO { MacroDef m }
  | ig=option(GLOBAL);stype=static_type;sname=LABEL;SEMICOLON
    { let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
      StaticVarUninitialized { is_global; stype; sname } }
  | ig=option(GLOBAL);stype=static_type;sname=LABEL;ASSIGN;value=static_value;SEMICOLON
    { begin match value with
      | StaticInteger i ->
        (match stype with
        | Byte -> if i > 0xFF then raise Exceptions.Integer_overflow
        | Word -> if i > 0xFFFF then raise Exceptions.Integer_overflow)
      | StaticString _ -> 
        (match stype with
        | Word -> raise Exceptions.String_as_words
        | _ -> ())
      end;
      let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
      StaticVar { is_global; stype; sname; value } }
  | EXTERN;externl=argument*;SEMICOLON { Extern externl }

function_type:
  | NEAR { Near }

static_type:
  | BYTE { Byte }
  | WORD { Word }

argument: lbl=LABEL;option(COMMA) { lbl }

func_decl: ig=option(GLOBAL);ftype=function_type;fname=LABEL;LPAREN;args=argument*;RPAREN
  { BatDynArray.add func_list { name=fname; args; locals=(BatDynArray.create ())}; 
    let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
    { is_global; ftype; fname; args } }

stmt:
  | IF;i=expr;LBRACE;t=stmt*;RBRACE { If (i,t) }
  | m=MACRO { MacroStmt m }
  | LET;l=LABEL;SEMICOLON 
    { let current_func = BatDynArray.last func_list in
      BatDynArray.add current_func.locals l;
      LocalVar (l, (Integer 0)) }
  | LET;l=LABEL;ASSIGN;v=value;SEMICOLON 
    { let current_func = BatDynArray.last func_list in
      BatDynArray.add current_func.locals l;
      LocalVar (l, v) }

expr:
  | LPAREN;lv=value;EQ;rv=value;RPAREN { Eq (lv,rv) }
  | v=value { Value v }
  | LPAREN;v=value;RPAREN { Value v }

value:
  | i=INTEGER { Integer i }
  | s=STRING { String s }
  | v=LABEL { Variable v }
  | address=address_value;LBRACK;offset=offset_value;RBRACK { Subscript (address,offset) }
  | ASTERISK;address=address_value { Subscript (address,(IntegerOffset 0)) }

offset_value:
  | i=INTEGER { IntegerOffset i }
  | v=LABEL { VariableOffset v }

address_value:
  | i=INTEGER { if i > 0xFFFFF then raise Exceptions.Pointer_overflow;
                IntegerAddress (((Int.shift_right i 16) * 0x1000),(Int.logand i 0xFFFF)) }
  | v=LABEL { VariableAddress v }

static_value:
  | i=INTEGER { StaticInteger i }
  | s=STRING { StaticString s }
