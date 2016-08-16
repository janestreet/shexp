open Import

type t =
  | Exited   of int
  | Signaled of int

let sexp_of_t = function
  | Exited   n -> Sexp.(cstr "Exited"   [int n])
  | Signaled n -> Sexp.(cstr "Signaled" [int n])
