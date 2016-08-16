(** Shell jobs, as threads to be portable *)

type 'a t

val detach : f:(unit -> 'a) -> 'a t
val wait   : 'a t -> 'a
