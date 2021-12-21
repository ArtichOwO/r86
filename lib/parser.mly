%{
  open Ast
%}

%token SEMICOLON
%token COMMA
%token EOF

%token <string> MACRO

%token LPAREN RPAREN
%token LBRACE RBRACE

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
    | is_global=option(GLOBAL);ftype=function_type;fname=label;LPAREN;args=argument*;RPAREN;
      LBRACE;stmt_list=stmt*;RBRACE 
        { FuncDef { is_global; ftype; fname; args; stmt_list } }
    | m=MACRO { MacroDef m }
    | is_global=option(GLOBAL);stype=static_type;sname=label;SEMICOLON
        { StaticVarUninitialized { is_global; stype; sname } }
    | is_global=option(GLOBAL);stype=static_type;sname=label;ASSIGN;value=value;SEMICOLON
        { StaticVar { is_global; stype; sname; value } }
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
    | v=label { Variable v }
    | LPAREN;v=label;RPAREN { Variable v }

value:
    | i=INTEGER { Integer i }
