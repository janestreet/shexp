(** OCaml portable signal id. These are normally negative numbers and they don't
    correspond to system signal ids. *)

type t = int

val name : t -> string
