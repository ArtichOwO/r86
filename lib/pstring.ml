open Asm_types
open Asm

type t = {
  header : instruction list;
  text : instruction list;
  data : instruction list;
  rodata : instruction list;
  bss : instruction list;
}

let create ?(header = []) ?(text = []) ?(data = []) ?(rodata = []) ?(bss = [])
    () =
  { header; text; data; rodata; bss }

let to_string { header; text; data; rodata; bss } =
  let map_to_string section =
    List.map Instruction.to_string section |> String.concat ""
  in
  List.map map_to_string [ header; text; data; rodata; bss ]
  |> String.concat "\n"

let headers =
  {
    header = [ Comment (false, Sys.argv.(1)); Bits 16; Newline ];
    text = [ Section ".text"; Newline ];
    data = [ Section ".data"; Newline ];
    rodata = [ Section ".rodata"; Newline ];
    bss = [ Section ".bss"; Newline ];
  }

let concat pstring_list =
  let fusion l r =
    {
      header = l.header @ r.header;
      text = l.text @ r.text;
      data = l.data @ r.data;
      rodata = l.rodata @ r.rodata;
      bss = l.bss @ r.bss;
    }
  in
  List.fold_left fusion (create ()) pstring_list
