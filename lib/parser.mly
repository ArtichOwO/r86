%{
  open Ast_types
%}

%token SEMICOLON
%token COMMA
%token EOF

%token <string> MACRO

%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK

%token ASTERISK

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
    | ig=option(GLOBAL);ftype=function_type;fname=label;LPAREN;args=argument*;RPAREN;
      LBRACE;stmt_list=stmt*;RBRACE 
        { let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
          FuncDef { is_global; ftype; fname; args; stmt_list } }
    | m=MACRO { MacroDef m }
    | ig=option(GLOBAL);stype=static_type;sname=label;SEMICOLON
        { let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
          StaticVarUninitialized { is_global; stype; sname } }
    | ig=option(GLOBAL);stype=static_type;sname=label;ASSIGN;value=static_value;SEMICOLON
        { let is_global = Option.fold ~none:false ~some:(fun _ -> true) ig in
          StaticVar { is_global; stype; sname; value } }
    | EXTERN;externl=argument*;SEMICOLON { Extern externl }

function_type:
    | NEAR { Near }

static_type:
    | BYTE { Byte }
    | WORD { Word }

argument: lbl=label;option(COMMA) { lbl }

label: lbl=LABEL { lbl }

stmt:
    | IF;i=expr;LBRACE;t=stmt*;RBRACE { If (i,t) }
    | m=MACRO { MacroStmt m }

expr:
    | LPAREN;lv=value;EQ;rv=value;RPAREN { Eq (lv,rv) }
    | v=value { Value v }
    | LPAREN;v=value;RPAREN { Value v }

value:
    | i=INTEGER { Integer i }
    | v=label { Variable v }
    | address=address_value;LBRACK;offset=offset_value;RBRACK { Subscript (address,offset) }
    | ASTERISK;address=address_value { Subscript (address,(IntegerOffset 0)) }

offset_value:
    | i=INTEGER { IntegerOffset i }
    | v=label { VariableOffset v }

address_value:
    | i=INTEGER { if i > 0xFFFFF then raise Exceptions.Pointer_overflow;
                  IntegerAddress (((Int.shift_right i 16) * 0x1000),(Int.logand i 0xFFFF)) }
    | v=label { VariableAddress v }

static_value:
    | i=INTEGER { StaticInteger i }
