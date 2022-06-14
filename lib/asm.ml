open Ast_types
open Asm_types
open Printf

module Size = struct
  let to_string = function Byte -> "byte" | Word -> "word"
end

module Static = struct
  let to_string = function
    | StaticInteger i -> sprintf "%#x" i
    | StaticLabel l -> l
    | StaticString s ->
        let convert_char buf chr =
          Buffer.add_string buf
            (match chr with
            | '\x00' -> "0,"
            | '\'' -> sprintf "%S," "'"
            | '\x20' .. '\x7E' -> sprintf "%C," chr
            | _ -> sprintf "%#x," (Char.code chr));
          buf
        in
        let buf =
          Seq.fold_left convert_char (Buffer.create 101) (String.to_seq s)
        in
        Buffer.add_char buf '0';
        Buffer.contents buf
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
    | OpInt i -> sprintf "%#x" i
    | OpLabel l -> l
    | OpChar c ->
        let to_int base nc = Int.shift_left base 4 + Char.code nc in
        let integer = BatString.fold_left to_int 0 c in
        sprintf "%#x" integer
end

module Instruction = struct
  let to_string instr =
    sprintf "%s\n"
      (match instr with
      | Newline -> ""
      | Text t -> sprintf "    %s" t
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
      | Iret -> "    iret"
      | Jmpn address -> sprintf "    jmp %s" (Operand.to_string address)
      | Jmpf (segment, address) ->
          sprintf "    jmp 0x%x:%s" segment (Operand.to_string address)
      | Jne address -> sprintf "    jne %s" (Operand.to_string address)
      | Je address -> sprintf "    je %s" (Operand.to_string address)
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
      | Mul src -> sprintf "    mul %s" (Operand.to_string src)
      | Div src -> sprintf "    div %s" (Operand.to_string src)
      | And (dst, src) ->
          sprintf "    and %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
      | Or (dst, src) ->
          sprintf "    or %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
      | Xor (dst, src) ->
          sprintf "    xor %s, %s" (Operand.to_string dst)
            (Operand.to_string src)
      | Not src -> sprintf "    not %s" (Operand.to_string src)
      | Calln address -> sprintf "    call %s" (Operand.to_string address)
      | Callf (segment, address) ->
          sprintf "    call %s:%s"
            (Operand.to_string segment)
            (Operand.to_string address))
end
