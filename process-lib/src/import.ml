module List = struct
  include ListLabels

  let concat_map l ~f = map l ~f |> concat
end

include (StdLabels : module type of struct include StdLabels end with module List := StdLabels.List)
include MoreLabels

module SMap = struct
  include Map.Make(String)
  let lookup t key =
    match find t key with
    | x -> Some x
    | exception Not_found -> None
end
module SSet = Set.Make(String)

include Shexp_sexp.Std
include Shexp_bigstring_io.Std

external reraise : exn -> _ = "%reraise"

let protectx ~finally ~f x =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; reraise e

let sprintf = Printf.sprintf

let ( ^/ ) = Filename.concat

module W : sig
  type 'a t
  val unit : unit t
  val int : int t
  val string : string t
  val process_status : Unix.process_status t
  val fd : Unix.file_descr t
  val stats : Unix.LargeFile.stats t
  val dir_handle : Unix.dir_handle t
  val pair : 'a t -> 'b t -> ('a * 'b) t
end = struct
  type 'a t = unit
  let unit = ()
  let int = ()
  let string = ()
  let process_status = ()
  let fd = ()
  let stats = ()
  let dir_handle = ()
  let pair _ _ = ()
end

(* We require a witness to be sure we apply the function fully *)
let retry_eintr : 'a. 'a W.t -> (unit -> 'a) -> 'a =
  let [@inline always] rec loop f n =
    try
      f ()
    with Unix.Unix_error (EINTR, _, _) when n < 1000 ->
      loop f (n + 1)
  in
  fun _ f -> loop f 0

let retry_eintr1 w f x = retry_eintr w (fun () -> f x)
let retry_eintr2 w f x y = retry_eintr w (fun () -> f x y)
let retry_eintr3 w f x y z = retry_eintr w (fun () -> f x y z)

module Unix = struct
  open Unix

  type nonrec file_descr        = file_descr
  type nonrec stats             = LargeFile.stats
  type nonrec access_permission = access_permission

  exception Unix_error = Unix_error

  let getpid      = getpid
  let environment = environment
  let stdin       = stdin
  let stdout      = stdout
  let stderr      = stderr
  let sleepf      = sleepf

  let close    x     = retry_eintr1 W.unit                      close           x
  let openfile x y z = retry_eintr3 W.fd                        openfile        x y z
  let readlink x     = retry_eintr1 W.string                    readlink        x
  let mkdir    x y   = retry_eintr2 W.unit                      mkdir           x y
  let chmod    x y   = retry_eintr2 W.unit                      chmod           x y
  let chown    x y z = retry_eintr3 W.unit                      chown           x y z
  let unlink   x     = retry_eintr1 W.unit                      unlink          x
  let rmdir    x     = retry_eintr1 W.unit                      rmdir           x
  let mkfifo   x y   = retry_eintr2 W.unit                      mkfifo          x y
  let link     x y   = retry_eintr2 W.unit                      link            x y
  let symlink  x y   = retry_eintr2 W.unit                      symlink         x y
  let rename   x y   = retry_eintr2 W.unit                      rename          x y
  let stat     x     = retry_eintr1 W.stats                     LargeFile.stat  x
  let lstat    x     = retry_eintr1 W.stats                     LargeFile.lstat x
  let access   x y   = retry_eintr2 W.unit                      access          x y
  let readdir  x     = retry_eintr1 W.string                    readdir         x
  let opendir  x     = retry_eintr1 W.dir_handle                opendir         x
  let closedir x     = retry_eintr1 W.unit                      closedir        x
  let lseek    x y z = retry_eintr3 W.int                       lseek           x y z
  let waitpid  x y   = retry_eintr2 W.(pair int process_status) waitpid         x y
end

module Posixat = struct
  open Posixat

  module Fd                = Fd
  module Open_flag         = Open_flag
  module At_flag           = At_flag
  module Access_permission = Access_permission
  module File_kind         = File_kind
  module File_perm         = File_perm
  module Stats             = Stats

  let at_fdcwd     = at_fdcwd
  let has_mkfifoat = has_mkfifoat

  let openat ~dir ~path ~flags ~perm = retry_eintr W.fd (fun () ->
    openat ~dir ~path ~flags ~perm)

  let faccessat ~dir ~path ~mode ~flags = retry_eintr W.unit (fun () ->
    faccessat ~dir ~path ~mode ~flags)

  let fchmodat ~dir ~path ~perm ~flags = retry_eintr W.unit (fun () ->
    fchmodat ~dir ~path ~perm ~flags)

  let fchownat ~dir ~path ~uid ~gid ~flags = retry_eintr W.unit (fun () ->
    fchownat ~dir ~path ~uid ~gid ~flags)

  let mkdirat ~dir ~path ~perm = retry_eintr W.unit (fun () ->
    mkdirat ~dir ~path ~perm)

  let unlinkat ~dir ~path ~flags = retry_eintr W.unit (fun () ->
    unlinkat ~dir ~path ~flags)

  let mkfifoat ~dir ~path ~perm = retry_eintr W.unit (fun () ->
    mkfifoat ~dir ~path ~perm)

  let linkat ~olddir ~oldpath ~newdir ~newpath ~flags = retry_eintr W.unit (fun () ->
    linkat ~olddir ~oldpath ~newdir ~newpath ~flags)

  let renameat ~olddir ~oldpath ~newdir ~newpath = retry_eintr W.unit (fun () ->
    renameat ~olddir ~oldpath ~newdir ~newpath)

  let symlinkat ~oldpath ~newdir ~newpath = retry_eintr W.unit (fun () ->
    symlinkat ~oldpath ~newdir ~newpath)

  let fstatat ~dir ~path ~flags = retry_eintr W.stats (fun () ->
    fstatat ~dir ~path ~flags)

  let readlinkat ~dir ~path = retry_eintr W.string (fun () ->
    readlinkat ~dir ~path)

  let fdopendir fd = retry_eintr1 W.dir_handle fdopendir fd
end
