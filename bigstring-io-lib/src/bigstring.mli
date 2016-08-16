(** Bigstring IO helpers *)

include module type of struct include Shexp_bigstring.Std.Bigstring end

val write : Unix.file_descr -> t -> pos:int -> len:int -> int
val read  : Unix.file_descr -> t -> pos:int -> len:int -> int

val read_all : Unix.file_descr -> string
val write_all : Unix.file_descr -> string -> unit

val read_exactly : Unix.file_descr -> int -> string

type read_all_interruptible_result =
  { interrupted : bool
  ; collected   : string
  }

(** Same as [read_all] except that [stop] is called every [delay] seconds until it returns
    [true]. In this case returns prematurely the data collected so far. *)
val read_all_interruptible
  :  ?delay:float (** default: 1. *)
  -> Unix.file_descr
  -> stop:(unit -> bool)
  -> read_all_interruptible_result

val fold_lines : Unix.file_descr -> init:'a -> f:('a -> string -> 'a) -> 'a
val fold_chunks : Unix.file_descr -> sep:char -> init:'a -> f:('a -> string -> 'a) -> 'a
