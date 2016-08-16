open Shexp_sexp.Std

module Fd = struct
  type t = Unix.file_descr

  type info =
    | Win32_handle of int64
    | Win32_socket of int64
    | Unix_fd of int

  external info : t -> info = "shexp_fd_info"

  let sexp_of_t t =
    match info t with
    | Unix_fd n -> Sexp.int n
    | Win32_handle h -> Sexp.cstr "HANDLE" [Atom (Printf.sprintf "0x%Lx" h)]
    | Win32_socket s -> Sexp.cstr "SOCKET" [Atom (Printf.sprintf "%Lx" s)]
end

module Open_flag = struct
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

  let sexp_of_t t =
    Sexp.Atom
      (match t with
       | O_RDONLY       -> "O_RDONLY"
       | O_WRONLY       -> "O_WRONLY"
       | O_RDWR         -> "O_RDWR"
       | O_NONBLOCK     -> "O_NONBLOCK"
       | O_APPEND       -> "O_APPEND"
       | O_CREAT        -> "O_CREAT"
       | O_TRUNC        -> "O_TRUNC"
       | O_EXCL         -> "O_EXCL"
       | O_NOCTTY       -> "O_NOCTTY"
       | O_DSYNC        -> "O_DSYNC"
       | O_SYNC         -> "O_SYNC"
       | O_RSYNC        -> "O_RSYNC"
       | O_SHARE_DELETE -> "O_SHARE_DELETE"
       | O_CLOEXEC      -> "O_CLOEXEC")
end

module At_flag = struct
  type t =
    | AT_EACCESS
    | AT_SYMLINK_FOLLOW
    | AT_SYMLINK_NOFOLLOW
    | AT_REMOVEDIR

  let sexp_of_t t =
    Sexp.Atom
      (match t with
       | AT_EACCESS          -> "AT_EACCESS"
       | AT_SYMLINK_FOLLOW   -> "AT_SYMLINK_FOLLOW"
       | AT_SYMLINK_NOFOLLOW -> "AT_SYMLINK_NOFOLLOW"
       | AT_REMOVEDIR        -> "AT_REMOVEDIR")
end

module Access_permission = struct
  type t = Unix.access_permission =
    | R_OK
    | W_OK
    | X_OK
    | F_OK

  let sexp_of_t t =
    Sexp.Atom
      (match t with
       | R_OK -> "R_OK"
       | W_OK -> "W_OK"
       | X_OK -> "X_OK"
       | F_OK -> "F_OK")
end

module File_perm = struct
  type t = Unix.file_perm

  let sexp_of_t t = Sexp.Atom (Printf.sprintf "0o%3o" t)
end

module File_kind = struct
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  let sexp_of_t t =
    Sexp.Atom
      (match t with
       | S_REG  -> "S_REG"
       | S_DIR  -> "S_DIR"
       | S_CHR  -> "S_CHR"
       | S_BLK  -> "S_BLK"
       | S_LNK  -> "S_LNK"
       | S_FIFO -> "S_FIFO"
       | S_SOCK -> "S_SOCK")
end

module Stats = struct
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

  let sexp_of_t t =
    Sexp.(
      record
        [ "st_dev"   , int t.st_dev
        ; "st_ino"   , int t.st_ino
        ; "st_kind"  , File_kind.sexp_of_t t.st_kind
        ; "st_perm"  , File_perm.sexp_of_t t.st_perm
        ; "st_nlink" , int t.st_nlink
        ; "st_uid"   , int t.st_uid
        ; "st_gid"   , int t.st_gid
        ; "st_rdev"  , int t.st_rdev
        ; "st_size"  , int64 t.st_size
        ; "st_atime" , float t.st_atime
        ; "st_mtime" , float t.st_mtime
        ; "st_ctime" , float t.st_ctime
        ])
end
