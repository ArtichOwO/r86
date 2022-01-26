open Asm_types

type t

val create :
  ?header:instruction list ->
  ?text:instruction list ->
  ?data:instruction list ->
  ?rodata:instruction list ->
  ?bss:instruction list ->
  unit ->
  t

val to_string : t -> string

val headers : t

val concat : t list -> t
