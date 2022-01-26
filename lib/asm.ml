open Ast_types
open Asm_types

module Size = struct
  let to_string = function Byte -> "byte" | Word -> "word"
end

module Static = struct
  let to_string = function
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
    | Memn address -> Printf.sprintf "[%s]" (to_string address)
    | Memf (segment, address) ->
        Printf.sprintf "[%s:%s]" (to_string segment) (to_string address)
    | MemnPos (address, offset) ->
        Printf.sprintf "[%s+%s]" (to_string address) (to_string offset)
    | MemnNeg (address, offset) ->
        Printf.sprintf "[%s-%s]" (to_string address) (to_string offset)
    | MemfPos (segment, address, offset) ->
        Printf.sprintf "[%s:%s+%s]" (to_string segment) (to_string address)
          (to_string offset)
    | MemfNeg (segment, address, offset) ->
        Printf.sprintf "[%s:%s-%s]" (to_string segment) (to_string address)
          (to_string offset)
    | OpInt i -> Printf.sprintf "0x%x" i
    | OpLabel l -> l
    | OpChar c ->
        let to_int base nc = Int.shift_left base 4 + Char.code nc in
        let integer = BatString.fold_left to_int 0 c in
        Printf.sprintf "0x%x" integer
end

module Instruction = struct
  let to_string instr =
    let instr_str =
      match instr with
      | Newline -> ""
      | Comment (indent, s) ->
          if indent then Printf.sprintf "    ; %s" s
          else Printf.sprintf "; %s" s
      | Global globals -> Printf.sprintf "GLOBAL %s" (String.concat "," globals)
      | Extern externs -> Printf.sprintf "EXTERN %s" (String.concat "," externs)
      | Section section -> Printf.sprintf "SECTION %s" section
      | Bits bits -> Printf.sprintf "BITS %i" bits
      | LabelDef (indent, s) ->
          if indent then Printf.sprintf "    %s:" s else Printf.sprintf "%s:" s
      | Mov (stype, dst, src) ->
          Printf.sprintf "    mov %s %s, %s" (Size.to_string stype)
            (Operand.to_string dst) (Operand.to_string src)
      | Cmp (stype, op1, op2) ->
          Printf.sprintf "    cmp %s %s, %s" (Size.to_string stype)
            (Operand.to_string op1) (Operand.to_string op2)
      | Push (stype, op) ->
          Printf.sprintf "    push %s %s" (Size.to_string stype)
            (Operand.to_string op)
      | Pop (stype, op) ->
          Printf.sprintf "    pop %s %s" (Size.to_string stype)
            (Operand.to_string op)
      | Retn -> "    retn"
      | Retf -> "    retf"
      | Jmpn address -> Printf.sprintf "    jmp %s" (Operand.to_string address)
      | Jmpf (segment, address) ->
          Printf.sprintf "    jmp 0x%x:%s" segment (Operand.to_string address)
      | Jne address -> Printf.sprintf "    jne %s" (Operand.to_string address)
      | Jnz address -> Printf.sprintf "    jnz %s" (Operand.to_string address)
      | Resb bytes -> Printf.sprintf "    resb 0x%x" bytes
      | Resw bytes -> Printf.sprintf "    resw 0x%x" bytes
      | Db svalue -> Printf.sprintf "    db %s" (Static.to_string svalue)
      | Dw svalue -> Printf.sprintf "    dw %s" (Static.to_string svalue)
      | Add (dst, src) ->
          Printf.sprintf "    add %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
      | Sub (dst, src) ->
          Printf.sprintf "    sub %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
    in
    Printf.sprintf "%s\n" instr_str
end
