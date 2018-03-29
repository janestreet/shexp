(** The Shexp process monad *)

open Shexp_sexp.Std

(** An ['a t] value represent a process pipeline description, that can be evaluated using
    [eval] into a value of type ['a].  Note that creating an ['a t] value has no
    effect. Effects are only performed when calling [eval].
*)
type 'a t

(** Execution contexts *)
module Context : sig
  (** An evaluation context represent all the information maintained by shexp_process in
      order to evaluate a process description.

      This consists of:
      - stdin, stdout, stderr
      - the current working directory
      - the unix environment variables
  *)
  type t

  module Working_dir : sig
    type t =
      | Inherit
      | Path of string
      | Physical of { path : string
                    ; fd   : Unix.file_descr
                    }
  end

  (** Creates a new execution context. Non-specified fields are captured from the
      environment of current running program. *)
  val create
    :  ?stdin:Unix.file_descr
    -> ?stdout:Unix.file_descr
    -> ?stderr:Unix.file_descr
    -> ?cwd:Working_dir.t
    -> ?unix_env:(string * string) list
    -> unit
    -> t

  (** In order to safely maintain several working directories inside the same system
      process, shexp keeps a file descriptor of the working directory of the context. You
      need to dispose of an execution context in order to close this file descriptor. *)
  val dispose : t -> unit
end

(** Evaluate the given process in the given environment.

    If [context] is not specified, a temporary one is created. If you are calling [eval]
    many times, then creating a context and reusing it is more efficient.
*)
val eval : ?context:Context.t -> 'a t -> 'a

module Logged : sig
  (** Call the given log function for every event. The log function default to printing to
      stderr.

      If [capture] is [true], then anything printed on the original [stdout]/[stderr] is
      captured and becomes part of the trace.  *)
  val eval
    :  ?context:Context.t
    -> ?capture:bool (** default: false *)
    -> ?log:(Sexp.t -> unit)
    -> 'a t
    -> 'a
end

module Traced : sig
  (** Produce a full execution trace.

      If [capture] is [true], then anything printed on the original [stdout]/[stderr] is
      captured and becomes part of the trace.  *)
  val eval
    :  ?context:Context.t
    -> ?capture:bool (** default: false *)
    -> 'a t
    -> ('a, exn) result * Sexp.t

  val eval_exn
    :  ?context:Context.t
    -> ?capture:bool (** default: false *)
    -> 'a t
    -> 'a * Sexp.t
end

module Prim : sig
  (** Description of a primitive *)
  type ('a, 'b) t = ('a, 'b) Prim.t

  module Args : sig
    (** Packed argument of a primitive call *)
    type ('a, 'b) t = ('a, 'b) Prim.Args.t
  end

  (** Return an s-expression representation of a primitive call *)
  val sexp_of_call : ('a, 'b) t -> ('a, 'b) Args.t -> Sexp.t

  (** Returns [None] if the result is [Unit] or [Env] *)
  val sexp_of_result : ('a, 'b) t -> 'b -> Sexp.t option
end

(** shexp_process allows one to plug a debugger in the evaluator. [Logged] and [Traced]
    are essentially two non-interactive debuggers. *)
module type Debugger = Debugger_intf.S

module With_debug(Dbg : Debugger) : sig
  val eval
    :  ?context:Context.t
    -> ?capture:bool (** default: false *)
    -> 'a t
    -> debugger:Dbg.t
    -> 'a
end

(** {1 Basic processes} *)

(** Classic monad operations. Note that because shexp_process uses threads under the hood,
    you should be careful about using global mutable data structures. To communicate
    values between concurrent processes, use the [sync] function. *)
val return : 'a -> 'a t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t

(** Create a process that fails with the given exception. Evaluation of this process will
    raise this exception. *)
val fail : exn -> _ t

(** [fork a b] reprensents two processes that are executed concurrently. The resulting
    process will block until both [a] and [b] have finished and will return both of their
    results.

    This is essentially the same as forking a system process: both process can
    independently change the current working directory, change their standard out, etc...

    Regarding errors, if the evaluation of both processes fail, then only one the
    exceptions will be kept. It is not specified which one. You should use [Traced] to get
    a full trace and see what exceptions are raised and where.
*)
val fork : 'a t -> 'b t -> ('a * 'b) t

(** Same as [map (fork a b) ~f:(fun (x, ()) -> x)] *)
val fork_unit : 'a t -> unit t -> 'a t

(** [protect ~finally t] protects against execution errors. [finally] is always executed,
    even if the evaluation of [t] raises. *)
val protect : finally:unit t -> 'a t -> 'a t

(** Capture exceptions into the process monad. You can use this if the argument of
    [protect] is the result of a function call.

    For instance in the following code [finally] wouldn't be executed:

    {[
      let f () = raise Not_found

      protect ~finally (f ())
    ]}

    You can write instead:

    {[
      protect ~finally (reify_exn f ())
    ]}
*)
val reify_exn : ('a -> 'b t) -> 'a -> 'b t

(** {1 Running commands} *)

(** Run an external program. This will fail if the external program does not exists, is
    signaled or exit with a non-zero exit code. *)
val run : string -> string list -> unit t

(** Run an external program and return its exit code. This will fail if the external
    program is signaled. *)
val run_exit_code : string -> string list -> int t

module Exit_status = Exit_status

(** Run an external program and return its exit status. *)
val run_exit_status : string -> string list -> Exit_status.t t

(** Run an external program, returns [true] if its exit code is part of [true_v] and
    [false] if it is part of [false_v]. *)
val run_bool
  :  ?true_v:int list (** default: [\[0\]] *)
  -> ?false_v:int list (** default: [\[1\]] *)
  -> string
  -> string list
  -> bool t

(** Same functions as the 'run' ones above, but take a string list instead. This
    way, the first element and the others are treated in a homogeneous way. It
    can ease prepending commands in specific circumstances, e.g.
    [if profile then call ("time" :: command) else call command]

    E.g. [call ["grep"; "-i"; pattern filename]] is equivalent to [run "grep"
    ["-i"; pattern; filename]] *)
val call : string list -> unit t
val call_exit_code : string list -> int t
val call_exit_status : string list -> Exit_status.t t
val call_bool
  :  ?true_v:int list
  -> ?false_v:int list
  -> string list
  -> bool t

module Background_command : sig
  type t

  val pid  : t -> int
end

(** Start an external program but do not wait for its termination. If you never call
    [wait] on the result, the process will become a zombie after it terminates. *)
val spawn : string -> string list -> Background_command.t t

(** Wait for a background command to terminate and return its exit status. *)
val wait : Background_command.t -> Exit_status.t t

(** {1 Unix environment} *)

(** Return the absolute path to the given command. *)
val find_executable : string -> string option t
val find_executable_exn : string -> string t

(** Return the value associated to the given environment variable. *)
val get_env : string -> string option t
val get_env_exn : string -> string t

(** [set_env var value k] represents the process [k] evaluated in a context where the
    envornment variable [var] is set to [value]. *)
val set_env : string -> string -> 'a t -> 'a t

(** [set_env var value k] represents the process [k] evaluated in a context where the
    environment variable [var] is unset. *)
val unset_env : string -> 'a t -> 'a t

(** {1 Current working directory} *)

(** Return the current working directory. Note that there is no guarantee that this
    directory exists. For instance if a component in this path has is renamed during
    the evaluation of the process, then this will be a dangling directory. *)
val cwd_logical : string t

(** [chdir dir k] represents the process [k] evaluated in a context where the current
    working directory is changed to [dir]. *)
val chdir : string -> 'a t -> 'a t

(** {1 IO} *)

module Std_io = Std_io

(** Output a string on one of the standard io. [~n:()] suppresses the newline output at
    the end. *)
val echo : ?where:Std_io.t -> ?n:unit -> string -> unit t

(** [echo ~where:Stdout] *)
val print : string -> unit t

(** [echo ~where:Stderr] *)
val eprint : string -> unit t

val printf : ('a, unit, string, unit t) format4 -> 'a
val eprintf : ('a, unit, string, unit t) format4 -> 'a

(** Consume all standard input *)
val read_all : string t

(** Fold over lines in the input. [f] is given the line with the end of line. Both
    ["\r\n"] and ["\n"] are treated as end of lines. *)
val fold_lines : init:'a -> f:('a -> string -> 'a t) -> 'a t

(** Fold over chunks separated by [sep] in the input. This can be used in conjunction
    with commands that support ending entries in the output with a ['\000'] such as
    [find -print0]. *)
val fold_chunks : sep:char -> init:'a -> f:('a -> string -> 'a t) -> 'a t

val iter_lines : (string -> unit t) -> unit t
val iter_chunks : sep:char -> (string -> unit t) -> unit t

(** {1 Pipes} *)

(** [pipe ~connect:(aios, bio) a b] is a process obtain by connecting the [aios] of [a] to
    the [bio] of [b]. [a] and [b] are evaluated in parallel (as with [fork]).

    [(aio, bio)] defaults to [([Stdout], Stdin)].
*)
val pipe : ?connect:(Std_io.t list * Std_io.t) -> unit t -> 'a t -> 'a t

(** [pipe_pair a b] is a the same as [pipe] but returns the results of both [a] and
    [b]. *)
val pipe_both : ?connect:(Std_io.t list * Std_io.t) ->  'a t -> 'b t -> ('a * 'b) t

(** Same as [pipe ~connect:([Stderr], Stdin)]. *)
val epipe : unit t -> 'a t -> 'a t
val epipe_both : 'a t -> 'b t -> ('a * 'b) t

(** [capture ios t = pipe_both ~connect:(ios, Stdin) read_all] *)
val capture : Std_io.t list -> 'a t -> ('a * string) t

(** [capture_unit ios t = pipe ~connect:(ios, Stdin) read_all] *)
val capture_unit : Std_io.t list -> unit t -> string t

(** {1 Redirections} *)

(** [redirect ios ?perm ~flags filename t] redirects the following ios to a file in
    [t]. [perm] and [flags] are passed the same as for [Unix.openfile]. *)
val redirect
  :  Std_io.t list
  -> ?perm:int
  -> flags:Unix.open_flag list
  -> string
  -> 'a t
  -> 'a t

(** Convenient wrappers for [redirect] *)
val stdout_to : ?append:unit -> string -> 'a t -> 'a t
val stderr_to : ?append:unit -> string -> 'a t -> 'a t
val outputs_to : ?append:unit -> string -> 'a t -> 'a t
val stdin_from : string -> 'a t -> 'a t

(** Replace the given standard io by the given one. For instance to redirect stdout to
    stderr: [replace_io ~stdout:Stderr t] *)
val replace_io
  :  ?stdin:Std_io.t
  -> ?stdout:Std_io.t
  -> ?stderr:Std_io.t
  -> 'a t
  -> 'a t

(** [out_to_err t = replace_io ~stdout:Stderr t] *)
val out_to_err : 'a t -> 'a t

(** [err_to_out t = replace_io ~stderr:Stdout t] *)
val err_to_out : 'a t -> 'a t

(** {1 Temporary files & directory} *)

(** Return the current temporary directory *)
val temp_dir : string t

(** [set_temp_dir dir k] represents the process [k] evaluated in a context where the
    current temporary directory is set to [dir]. *)
val set_temp_dir : string -> 'a t -> 'a t

(** [with_temp_file ~prefix ~suffix f] is a process that creates a temporary file and
    passes it to [f]. The file is created inside the temporary directory.

    When the process returned by [f] finishes, the file is removed.
*)
val with_temp_file : prefix:string -> suffix:string -> (string -> 'a t) -> 'a t

(** Same as [with_temp_file] but creates a directory. The directory and its contents are
    deleted when the process finishes. *)
val with_temp_dir : prefix:string -> suffix:string -> (string -> 'a t) -> 'a t

(** {1 FS operations} *)

val chmod       : string -> perm:Unix.file_perm -> unit t
val chown       : string -> uid:int -> gid:int -> unit t
val mkdir       : ?perm:Unix.file_perm -> ?p:unit -> string -> unit t
val rm          : string -> unit t
val rmdir       : string -> unit t
val mkfifo      : ?perm:Unix.file_perm -> string -> unit t
val link        : string -> string -> unit t
val rename      : string -> string -> unit t
val symlink     : string -> string -> unit t
val stat        : string -> Unix.LargeFile.stats t
val lstat       : string -> Unix.LargeFile.stats t
val readlink    : string -> string t
val readdir     : string -> string list t
val file_exists : string -> bool t

(** Recursively remove a tree *)
val rm_rf : string -> unit t

(** {1 Misc} *)

val sleep : float -> unit t

(** {1 Synchronisation} *)

val new_channel : 'a Event.channel t
val sync : 'a Event.event -> 'a t

(** {1 Misc} *)

module Infix : sig
  (** Same as [bind] *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Same as [map] *)
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  (** [a >> b] is the same as [a >>= fun () -> b] *)
  val ( >> ) : unit t -> 'a t -> 'a t

  (** Same as [pipe] *)
  val ( |- ) : unit t -> 'a t -> 'a t

  (** Same as [pipe_both] *)
  val ( |+ ) : 'a t -> 'b t -> ('a * 'b) t
end

(** Open this module when using [ppx_let] *)
module Let_syntax : sig
  val return : 'a -> 'a t
  include (module type of Infix)

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map  : 'a t -> f:('a -> 'b) -> 'b t

    (** Same as [fork] *)
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end

module List : sig
  val iter : 'a list -> f:('a -> unit t) -> unit t
end
