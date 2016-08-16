open Import

type t =
  | Stdin
  | Stdout
  | Stderr

let sexp_of_t t =
  Sexp.Atom (match t with
    | Stdin  -> "stdin"
    | Stdout -> "stdout"
    | Stderr -> "stderr")
