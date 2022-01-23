type t = {
  header : string;
  text : string;
  data : string;
  rodata : string;
  bss : string;
}

let create ?(header = "") ?(text = "") ?(data = "") ?(rodata = "") ?(bss = "")
    () =
  { header; text; data; rodata; bss }

and to_string { header; text; data; rodata; bss } =
  String.concat "\n" [ header; text; data; rodata; bss ]

and headers =
  {
    header = Printf.sprintf "; %s \nBITS 16\n\n" Sys.argv.(1);
    text = "SECTION .text \n\n";
    data = "SECTION .data \n\n";
    rodata = "SECTION .rodata \n\n";
    bss = "SECTION .bss \n\n";
  }

and concat pstring_list =
  let concat (h, t, d, rd, b) { header; text; data; rodata; bss } =
    (ignore
    @@ Buffer.
         ( add_string h header,
           add_string t text,
           add_string d data,
           add_string rd rodata,
           add_string b bss ));
    (h, t, d, rd, b)
  and unbuf (h, t, d, rd, b) =
    {
      header = Buffer.contents h;
      text = Buffer.contents t;
      data = Buffer.contents d;
      rodata = Buffer.contents rd;
      bss = Buffer.contents b;
    }
  in
  List.fold_left concat
    Buffer.(create 17, create 17, create 17, create 17, create 17)
    pstring_list
  |> unbuf
