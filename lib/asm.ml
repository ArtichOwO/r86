open Ast_types
open Asm_types
open Printf

module Size = struct
  let to_string = function Byte -> "byte" | Word -> "word"
end

module Static = struct
  let to_string = function
    | StaticInteger i -> sprintf "0x%x" i
    | StaticString s ->
        let explode s = List.init (String.length s) (String.get s) in
        let convert_char c others =
          let new_char =
            match c with
            | '\x00' -> "0,"
            | '\x20' .. '\x7E' as c -> sprintf "\'%c\'," c
            | _ as c -> Char.code c |> sprintf "0x%x,"
          in
          new_char ^ others
        in
        let new_string = List.fold_right convert_char (explode s) "" in
        sprintf "%s0" new_string
end

module Operand = struct
  let rec to_string = function
    | Register reg -> (
        match reg with
        | AX -> "ax"
        | BX -> "bx"
        | CX -> "cx"
        | DX -> "dx"
        | AL -> "al"
        | BL -> "bl"
        | CL -> "cl"
        | DL -> "dl"
        | AH -> "ah"
        | BH -> "bh"
        | CH -> "ch"
        | DH -> "dh"
        | CS -> "cs"
        | DS -> "ds"
        | SS -> "ss"
        | ES -> "es"
        | SI -> "si"
        | DI -> "di"
        | SP -> "sp"
        | BP -> "bp")
    | Memn address -> sprintf "[%s]" (to_string address)
    | Memf (segment, address) ->
        sprintf "[%s:%s]" (to_string segment) (to_string address)
    | MemnPos (address, offset) ->
        sprintf "[%s+%s]" (to_string address) (to_string offset)
    | MemnNeg (address, offset) ->
        sprintf "[%s-%s]" (to_string address) (to_string offset)
    | MemfPos (segment, address, offset) ->
        sprintf "[%s:%s+%s]" (to_string segment) (to_string address)
          (to_string offset)
    | MemfNeg (segment, address, offset) ->
        sprintf "[%s:%s-%s]" (to_string segment) (to_string address)
          (to_string offset)
    | OpInt i -> sprintf "0x%x" i
    | OpLabel l -> l
    | OpChar c ->
        let to_int base nc = Int.shift_left base 4 + Char.code nc in
        let integer = BatString.fold_left to_int 0 c in
        sprintf "0x%x" integer
end

module Instruction = struct
  let to_string instr =
    let instr_str =
      match instr with
      | Newline -> ""
      | Comment (indent, s) ->
          if indent then sprintf "    ; %s" s else sprintf "; %s" s
      | Global globals -> sprintf "GLOBAL %s" (String.concat "," globals)
      | Extern externs -> sprintf "EXTERN %s" (String.concat "," externs)
      | Section section -> sprintf "SECTION %s" section
      | Bits bits -> sprintf "BITS %i" bits
      | LabelDef (indent, s) ->
          if indent then sprintf "    %s:" s else sprintf "%s:" s
      | Mov (stype, dst, src) ->
          sprintf "    mov %s %s, %s" (Size.to_string stype)
            (Operand.to_string dst) (Operand.to_string src)
      | Cmp (stype, op1, op2) ->
          sprintf "    cmp %s %s, %s" (Size.to_string stype)
            (Operand.to_string op1) (Operand.to_string op2)
      | Push (stype, op) ->
          sprintf "    push %s %s" (Size.to_string stype) (Operand.to_string op)
      | Pop (stype, op) ->
          sprintf "    pop %s %s" (Size.to_string stype) (Operand.to_string op)
      | Retn -> "    retn"
      | Retf -> "    retf"
      | Jmpn address -> sprintf "    jmp %s" (Operand.to_string address)
      | Jmpf (segment, address) ->
          sprintf "    jmp 0x%x:%s" segment (Operand.to_string address)
      | Jne address -> sprintf "    jne %s" (Operand.to_string address)
      | Jnz address -> sprintf "    jnz %s" (Operand.to_string address)
      | Jz address -> sprintf "    jz %s" (Operand.to_string address)
      | Resb bytes -> sprintf "    resb 0x%x" bytes
      | Resw bytes -> sprintf "    resw 0x%x" bytes
      | Db svalue -> sprintf "    db %s" (Static.to_string svalue)
      | Dw svalue -> sprintf "    dw %s" (Static.to_string svalue)
      | Add (dst, src) ->
          sprintf "    add %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
      | Sub (dst, src) ->
          sprintf "    sub %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
      | Calln address -> sprintf "    call %s" (Operand.to_string address)
      | Callf (segment, address) ->
          sprintf "    call %s:%s"
            (Operand.to_string segment)
            (Operand.to_string address)
    in
    sprintf "%s\n" instr_str
end
