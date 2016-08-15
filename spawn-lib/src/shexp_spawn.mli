(** Mini spawn library *)

module Working_dir : sig
  type t =
    | Path of string        (** Path in the filesystem *)
    | Fd of Unix.file_descr (** File descriptor pointing to a directory.
                                Not supported on Windows. *)
    | Inherit               (** Inherit the working directory of
                                the current process *)
end

(** Spawn a sub-command, using vfork+exec on unix and CreateProcess on windows. Returns
    the PID of the sub-process.

    Note: on Unix, the sub-process will have all its signals unblocked.
*)
val spawn
  :  ?env:string list
  -> ?cwd:Working_dir.t (** default: [Inherit] *)
  -> prog:string
  -> argv:string list
  -> ?stdin:Unix.file_descr
  -> ?stdout:Unix.file_descr
  -> ?stderr:Unix.file_descr
  -> unit
  -> int
