open Ast_types

type register =
  | AX
  | BX
  | CX
  | DX
  | AL
  | BL
  | CL
  | DL
  | AH
  | BH
  | CH
  | DH
  | CS
  | DS
  | SS
  | ES
  | SI
  | DI
  | SP
  | BP

and operand =
  | Register of register
  | Memn of operand
  | Memf of operand * operand
  | MemnPos of operand * operand
  | MemnNeg of operand * operand
  | MemfPos of operand * operand * operand
  | MemfNeg of operand * operand * operand
  | OpInt of int
  | OpLabel of string
  | OpChar of string

and instruction =
  | Newline
  | Text of string
  | Comment of bool * string
  | Global of string list
  | Extern of string list
  | Section of string
  | Bits of int
  | LabelDef of bool * string
  | Mov of size_type * operand * operand
  | Cmp of size_type * operand * operand
  | Push of size_type * operand
  | Pop of size_type * operand
  | Retn
  | Retf
  | Jmpn of operand
  | Jmpf of int * operand
  | Jne of operand
  | Je of operand
  | Jnz of operand
  | Jz of operand
  | Resb of int
  | Resw of int
  | Db of static_value
  | Dw of static_value
  | Add of operand * operand
  | Sub of operand * operand
  | Mul of operand
  | Div of operand
  | Calln of operand
  | Callf of operand * operand
