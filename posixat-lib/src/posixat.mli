(** Bindings for the *at family of POSIX functions *)

open Shexp_sexp.Std

module Fd : sig
  type t = Unix.file_descr

  val sexp_of_t : t -> Sexp.t
end

module Open_flag : sig
  type t = Unix.open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_APPEND
    | O_CREAT
    | O_TRUNC
    | O_EXCL
    | O_NOCTTY
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_SHARE_DELETE
    | O_CLOEXEC

  val sexp_of_t : t -> Sexp.t
end

module At_flag : sig
  type t =
    | AT_EACCESS
    | AT_SYMLINK_FOLLOW
    | AT_SYMLINK_NOFOLLOW
    | AT_REMOVEDIR

  val sexp_of_t : t -> Sexp.t
end

module Access_permission : sig
  type t = Unix.access_permission =
    | R_OK
    | W_OK
    | X_OK
    | F_OK

  val sexp_of_t : t -> Sexp.t
end

module File_kind : sig
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  val sexp_of_t : t -> Sexp.t
end

module File_perm : sig
  type t = int

  val sexp_of_t : t -> Sexp.t
end

module Stats : sig
  type t = Unix.LargeFile.stats =
    { st_dev   : int
    ; st_ino   : int
    ; st_kind  : File_kind.t
    ; st_perm  : File_perm.t
    ; st_nlink : int
    ; st_uid   : int
    ; st_gid   : int
    ; st_rdev  : int
    ; st_size  : int64
    ; st_atime : float
    ; st_mtime : float
    ; st_ctime : float
    }

  val sexp_of_t : t -> Sexp.t
end

val at_fdcwd : unit -> Fd.t

val openat
  :  dir:Fd.t
  -> path:string
  -> flags:Open_flag.t list
  -> perm:File_perm.t
  -> Fd.t

val faccessat
  :  dir:Fd.t
  -> path:string
  -> mode:Access_permission.t list
  -> flags:At_flag.t list
  -> unit

val fchmodat
  :  dir:Fd.t
  -> path:string
  -> perm:File_perm.t
  -> flags:At_flag.t list
  -> unit

val fchownat
  :  dir:Fd.t
  -> path:string
  -> uid:int
  -> gid:int
  -> flags:At_flag.t list
  -> unit

val mkdirat
  :  dir:Fd.t
  -> path:string
  -> perm:File_perm.t
  -> unit

val unlinkat
  :  dir:Fd.t
  -> path:string
  -> flags:At_flag.t list
  -> unit

val mkfifoat
  :  dir:Fd.t
  -> path:string
  -> perm:File_perm.t
  -> unit

val linkat
  :  olddir:Fd.t
  -> oldpath:string
  -> newdir:Fd.t
  -> newpath:string
  -> flags:At_flag.t list
  -> unit

val renameat
  :  olddir:Fd.t
  -> oldpath:string
  -> newdir:Fd.t
  -> newpath:string
  -> unit

val symlinkat
  :  oldpath:string
  -> newdir:Fd.t
  -> newpath:string
  -> unit

val fstatat
  :  dir:Fd.t
  -> path:string
  -> flags:At_flag.t list
  -> Stats.t

val readlinkat
  :  dir:Fd.t
  -> path:string
  -> string

val fdopendir : Fd.t -> Unix.dir_handle

val has_mkfifoat : bool
