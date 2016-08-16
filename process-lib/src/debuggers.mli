open Shexp_sexp.Std

module type S = Debugger_intf.S

(** Log events synchronously using the given printer. *)
module Logger : sig
  include S

  val create : (Sexp.t -> unit) -> t
end

(** Produce a full execution trace. The order of the trace is independant of the order in
    which command are executed in different threads, so is suitable for expect tests. *)
module Tracer : sig
  include S

  val create : unit -> t
  val result : t -> Sexp.t
end
