type t

val create :
  ?header:string ->
  ?text:string ->
  ?data:string ->
  ?rodata:string ->
  ?bss:string ->
  unit ->
  t

val to_string : t -> string

val headers : t

val concat : t list -> t
