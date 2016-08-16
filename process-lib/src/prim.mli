(** Specification of primitive types *)

open Shexp_sexp.Std

module Args : sig
  module Spec : sig
    module Arg : sig
      type 'a t =
        | A of ('a -> Sexp.t)
        | L of string * ('a -> Sexp.t)
          (** L for Labeled *)
        | O of string * ('a -> Sexp.t) * 'a
          (** O for Optional *)
    end

    type 'a t =
      | []     : 'a t
      | ( :: ) : 'a Arg.t * 'b t -> ('a -> 'b) t
  end

  type ('a, 'b) t =
    | A0 : unit -> ('a, 'a) t
    | A1 : 'a -> ('a -> 'b, 'b) t
    | A2 : 'a * 'b -> ('a -> 'b -> 'c, 'c) t
    | A3 : 'a * 'b * 'c -> ('a -> 'b -> 'c -> 'd, 'd) t
    | A4 : 'a * 'b * 'c * 'd  -> ('a -> 'b -> 'c -> 'd -> 'e, 'e) t
    | A5 : 'a * 'b * 'c * 'd * 'e -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f, 'f) t

  val apply : ('env -> 'a) -> 'env -> ('a, 'b) t -> 'b
end

module Result_spec : sig
  type 'a t =
    | Unit : unit t
    | Env  : Env.t t
    | F    : ('a -> Sexp.t) -> 'a t
end

(* The type is exposed to solve generalization problems *)
type ('a, 'b) t =
  { name   : string
  ; args   : 'a Args.Spec.t
  ; result : 'b Result_spec.t
  ; run    : Env.t -> 'a
  }

val make
  :  string
  -> 'a Args.Spec.t
  -> 'b Result_spec.t
  -> (Env.t -> 'a)
  -> ('a, 'b) t

val run : ('a, 'b) t -> Env.t -> ('a, 'b) Args.t -> 'b

val sexp_of_call : ('a, 'b) t -> ('a, 'b) Args.t -> Sexp.t

(** Returns [None] if the result is [Unit] or [Env] *)
val sexp_of_result : ('a, 'b) t -> 'b -> Sexp.t option
