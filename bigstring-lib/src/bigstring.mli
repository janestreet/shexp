(** Bigstring helpers for shexp libraries *)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

val create : int -> t

val length : t -> int

val blit
  :  src:t
  -> src_pos:int
  -> dst:t
  -> dst_pos:int
  -> len:int
  -> unit

val blit_string_t
  :  src:string
  -> src_pos:int
  -> dst:t
  -> dst_pos:int
  -> len:int
  -> unit

val blit_t_bytes
  :  src:t
  -> src_pos:int
  -> dst:Bytes.t
  -> dst_pos:int
  -> len:int
  -> unit

val sub_string : t -> pos:int -> len:int -> string

val index  : t -> pos:int -> len:int -> char:char -> int option
val rindex : t -> pos:int -> len:int -> char:char -> int option

(** Efficiently checks that the range denoted by [(pos, len)] is in the range
    [0..length]. [length] is assumed to be [>= 0]. *)
val pos_len_ok : pos:int -> len:int -> length:int -> bool

(** Raises if [(pos, len)] denotes a range outside of [0..length]. *)
val check_pos_len_exn : pos:int -> len:int -> length:int -> unit

(** Allocate a bigstring and pass it to [f]. The memory allocated for the bigstring is
    released as soon as [f] returns. As such, the bigstring shouldn't be used after [f]
    returns.

    This is more efficient than waiting on the garbage collector to release the external
    memory.
*)
val with_temporary : size:int -> f:(t -> 'a) -> 'a

type ('a, 'b) fold_temporary_result =
  | Resize   of { new_size : int; state : 'a }
  | Continue of { state : 'a } (** Same as [Resize] with the same size *)
  | Return of 'b

(** Same as [with_temporary], but allow to resize the bigstring if needed.

    If [f] returns [Resize { new_size; state }], the bigstring will be resized to the
    given new size and [f] will be called with the new bigstring and [state]. The contents
    of the bigstring up to the min of the old and new sizes is preserved.
*)
val fold_temporary
  :  size:int
  -> init:'a
  -> f:(t -> 'a -> ('a, 'b)  fold_temporary_result)
  -> 'b
