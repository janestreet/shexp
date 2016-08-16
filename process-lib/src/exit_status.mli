(** Exit status of external processes *)

open Shexp_sexp.Std

type t =
  | Exited   of int
  | Signaled of int

val sexp_of_t : t -> Sexp.t
