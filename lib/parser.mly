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

%token EQ NEQ
%token NEAR FAR INT

%token <string> STRING
%token <string> LABEL
%token <int> INTEGER

%start <program> program

%%

program: e=defs*; EOF { e }

defs:
    | pt=function_type;lbl=label;LPAREN;args=argument*;RPAREN;
      LBRACE;sl=stmt*;RBRACE 
        { FuncDef (pt,lbl,args,sl) }
    | m=MACRO { MacroDef m }

function_type:
    | NEAR { Near }

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
