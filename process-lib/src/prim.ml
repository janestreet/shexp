open Import

module Args = struct
  module Spec = struct
    module Arg = struct
      type 'a t =
        | A of ('a -> Sexp.t)
        | L of string * ('a -> Sexp.t)
        | O of string * ('a -> Sexp.t) * 'a
    end

    type 'a t =
      | []     : 'a t
      | ( :: ) : 'a Arg.t * 'b t -> ('a -> 'b) t
  end

  (* We use this rather than a generic list like structure to make [apply] faster. [A0] is
     not a constant for optimization purposes: matching on a [t] generates one less
     conditional jump. *)
  type ('a, 'b) t =
    | A0 : unit -> ('a, 'a) t
    | A1 : 'a -> ('a -> 'b, 'b) t
    | A2 : 'a * 'b -> ('a -> 'b -> 'c, 'c) t
    | A3 : 'a * 'b * 'c -> ('a -> 'b -> 'c -> 'd, 'd) t
    | A4 : 'a * 'b * 'c * 'd  -> ('a -> 'b -> 'c -> 'd -> 'e, 'e) t
    | A5 : 'a * 'b * 'c * 'd * 'e -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f, 'f) t

  let apply : type env a b. (env -> a) -> env -> (a, b) t -> b = fun f env t ->
    match t with
    | A0 ()              -> f env
    | A1 (a)             -> f env a
    | A2 (a, b)          -> f env a b
    | A3 (a, b, c)       -> f env a b c
    | A4 (a, b, c, d)    -> f env a b c d
    | A5 (a, b, c, d, e) -> f env a b c d e

  let sexps : type a b. a Spec.t -> (a, b) t -> Sexp.t list
    = fun spec args ->
      let open Spec in
      let arg (arg : _ Spec.Arg.t) x acc : Sexp.t list =
        match arg with
        | A f ->
          f x :: acc
        | L (name, f) ->
          List [ Atom name; f x ] :: acc
        | O (name, f, default) ->
          if x = default then
            acc
          else
            List [ Atom name; f x ] :: acc
      in
      match spec, args with
      | []           , A0 ()        ->                                  []
      | [sa]         , A1 (a)       -> arg sa a                         []
      | [sa; sb]     , A2 (a, b)    -> arg sa a @@ arg sb b             []
      | [sa; sb; sc] , A3 (a, b, c) -> arg sa a @@ arg sb b @@ arg sc c []
      | [sa; sb; sc; sd], A4 (a, b, c, d) ->
        arg sa a @@ arg sb b @@ arg sc c @@ arg sd d []
      | [sa; sb; sc; sd; se], A5 (a, b, c, d, e) ->
        arg sa a @@ arg sb b @@ arg sc c @@ arg sd d @@ arg se e []
      | _ -> invalid_arg "Shexp_process.Prim.Args.sexps"
end

module Result_spec = struct
  type 'a t =
    | Unit : unit t
    | Env  : Env.t t
    | F    : ('a -> Sexp.t) -> 'a t

  let sexp : type a. a t -> a -> Sexp.t option = fun t x ->
    match t with
    | Unit -> None
    | Env  -> None
    | F f  -> Some (f x)
end

type ('a, 'b) t =
  { name   : string
  ; args   : 'a Args.Spec.t
  ; result : 'b Result_spec.t
  ; run    : Env.t -> 'a
  }

let make name args result run =
  { name
  ; args
  ; result
  ; run
  }

let run t env args = Args.apply t.run env args

let sexp_of_call t args =
  Sexp.List (Atom t.name :: Args.sexps t.args args)

let sexp_of_result t x = Result_spec.sexp t.result x
