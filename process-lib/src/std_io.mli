open Shexp_sexp.Std

type t =
  | Stdin
  | Stdout
  | Stderr

val sexp_of_t : t -> Sexp.t
