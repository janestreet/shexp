(** Runtime environment

    If we were to compile a script to an OCaml program, all functions would have to take
    this environment as argument. *)

open Import

type t

module Working_dir_spec : sig
  type t =
    | Inherit
    | Path of string
    | Physical of { path : string
                  ; fd   : Unix.file_descr
                  }
end

val create
  :  ?stdin:Unix.file_descr
  -> ?stdout:Unix.file_descr
  -> ?stderr:Unix.file_descr
  -> ?cwd:Working_dir_spec.t
  -> ?unix_env:(string * string) list
  -> unit
  -> t

(** Increment the reference of the working directory *)
val add_cwd_ref : t -> unit

(** Decrement the reference of the working directory *)
val deref_cwd : t -> unit

(** Unix environment *)
val find_executable : t -> string -> string option
val get_env : t -> string -> string option
val set_env : t -> string -> string -> t
val unset_env : t -> string -> t

val set_env_many : t -> (string * string) list -> t
val unset_env_many : t -> string list -> t

val cwd_logical : t -> string
val chdir : t -> string -> t

val stdin : t -> Unix.file_descr
val stdout : t -> Unix.file_descr
val stderr : t -> Unix.file_descr

val set_stdin : t -> Unix.file_descr -> t
val set_stdout : t -> Unix.file_descr -> t
val set_stderr : t -> Unix.file_descr -> t
val set_outputs : t -> Unix.file_descr -> t
val set_stdios
  :  t
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> t

val get_stdio : t -> Std_io.t -> Unix.file_descr
val set_stdio : t -> Std_io.t -> Unix.file_descr -> t

type run_error =
  | Command_not_found

val spawn : t -> prog:string -> args:string list -> (int, run_error) result

val open_file
  :  t
  -> ?perm:Posixat.File_perm.t
  -> flags:Posixat.Open_flag.t list
  -> string
  -> Unix.file_descr

val with_file
  :  t
  -> ?perm:Posixat.File_perm.t
  -> flags:Posixat.Open_flag.t list
  -> string
  -> f:(Unix.file_descr -> 'a)
  -> 'a

val chmod    : t -> string -> perm:int -> unit
val chown    : t -> string -> uid:int -> gid:int -> unit
val mkdir    : t -> ?perm:int -> ?p:bool -> string -> unit
val rm       : t -> string -> unit
val rmdir    : t -> string -> unit
val mkfifo   : t -> ?perm:int -> string -> unit
val link     : t -> string -> string -> unit
val rename   : t -> string -> string -> unit
val symlink  : t -> string -> string -> unit
val stat     : t -> string -> Unix.stats
val lstat    : t -> string -> Unix.stats
val readlink : t -> string -> string
val readdir  : t -> string -> string list
val access   : t -> string -> Unix.access_permission list -> unit
