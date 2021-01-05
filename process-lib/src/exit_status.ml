open Import

type t =
  | Exited of int
  | Signaled of Signal.t

let sexp_of_t = function
  | Exited n -> Sexp.(cstr "Exited" [ int n ])
  | Signaled n -> Sexp.(cstr "Signaled" [ int n ])
;;
