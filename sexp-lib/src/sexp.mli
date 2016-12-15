(** Mini S-expression library *)

type t = Base.Exported_for_specific_uses.Sexplib.Sexp.t = Atom of string | List of t list

val to_string : t -> string

(** Combinators *)
val int : int -> t
val int32 : int32 -> t
val int64 : int64 -> t
val float : float -> t
val string : string -> t
val char : char -> t
val bool : bool -> t
val exn : exn -> t
val pair : ('a -> t) -> ('b -> t) -> 'a * 'b -> t
val list : ('a -> t) -> 'a list -> t
val option : ('a -> t) -> 'a option -> t
val record : (string * t) list -> t
val cstr : string -> t list -> t
val cstr_record : string -> (string * t) list -> t
val cstr_list : string -> ('a -> t) -> 'a list -> t

val register_exn_converter : extension_constructor -> (exn -> t) -> unit
