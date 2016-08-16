module type S = sig
  type t

  type ('a, 'b) prim_token

  (** Called before the execution of a primitive. The S-expression is a representation of
      the primitive call, for instance [(mkdir "foo")]. *)
  val before_prim : t -> ('a, 'b) Prim.t -> ('a, 'b) Prim.Args.t -> ('a, 'b) prim_token

  (** Called after the execution of a primitive, with the result. *)
  val after_prim
    :  t
    -> ('a, 'b) Prim.t
    -> ('b, exn * Printexc.raw_backtrace) result
    -> ('a, 'b) prim_token
    -> unit

  (** Register a user exception, i.e. when a [bind] raises. *)
  val user_exn : t -> exn -> Printexc.raw_backtrace -> unit

  (** Captured output *)
  val output : t -> string -> unit

  (** Called before forking. *)
  val fork : t -> t * t

  (** Called in the same thread that started the fork. *)
  val end_fork : t -> t -> t -> unit

  (** Shexp tries to linearize the trace as much as possible. When an execution parameter
      is changed locally, such as in [(chdir ...) >>= ...], a sub context is entered. *)
  val enter_sub : t -> unit
  val leave_sub : t -> unit

  (** Force the use of threads in all forks, even when [Shexp_process] detect they are not
      necessary. *)
  val force_threads : bool
end
