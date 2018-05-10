open Import

module Working_dir_spec = struct
  type t =
    | Inherit
    | Path of string
    | Physical of { path : string
                  ; fd   : Unix.file_descr
                  }
end

module Working_dir = struct
  (* Physical working directories are ref-counted so that the file descriptor can be
     closed as soon as it is not needed anymore. This allow to optimize a [chdir] in tail
     position. *)
  module Physical = struct
    type t =
      | Win32 (* Physical path is not supported on Windows *)
      | Unix of { fd            : Unix.file_descr
                ; mutable users : int
                ; mutex         : Mutex.t
                }

    let create_unix fd =
      Unix { fd; users = 1; mutex = Mutex.create () }

    let addref = function
      | Win32 -> ()
      | Unix u ->
        Mutex.lock u.mutex;
        let users = u.users + 1 in
        u.users <- users;
        Mutex.unlock u.mutex;
        (* One must not try to reference a physical path that has been released *)
        assert (users >= 2)

    let deref = function
      | Win32 -> ()
      | Unix u ->
        Mutex.lock u.mutex;
        let users = u.users - 1 in
        u.users <- users;
        Mutex.unlock u.mutex;
        assert (users >= 0);
        if users = 0 then try Unix.close u.fd with _ -> ()

    let map_unix t ~f =
      match t with
      | Win32 -> Win32
      | Unix u -> create_unix (f u.fd)
  end

  type t =
    { logical       : string
    ; physical      : Physical.t
    }

  let create (spec : Working_dir_spec.t) =
    let logical, physical =
      match spec with
      | Inherit -> (Sys.getcwd (), None)
      | Path p -> (p, None)
      | Physical { path; fd } -> (path, Some fd)
    in
    let physical : Physical.t =
      match physical, Sys.win32 with
      | Some _, true ->
        invalid_arg "Shexp_exec.Std.Env.create: cannot use a physical \
                     working directory on Windows";
      | Some fd, false ->
        Physical.create_unix fd
      | None, true ->
        Win32
      | None, false ->
        Physical.create_unix (Unix.openfile logical [O_CLOEXEC; O_RDONLY] 0)
    in
    { logical; physical }

  let split_relative_dir dir =
    let rec loop dir acc =
      let acc = Filename.basename dir :: acc in
      let dir = Filename.dirname dir in
      if dir = Filename.current_dir_name then
        acc
      else
        loop dir acc
    in
    loop dir []

  let chdir_logical dir relative_dir =
    let rec loop dir components ~symlinks_followed =
      match components with
      | [] -> dir
      | component :: rest ->
        if component = Filename.current_dir_name then
          loop dir rest ~symlinks_followed
        else if component = Filename.parent_dir_name then
          match Unix.readlink dir with
          | exception (Unix.Unix_error (EINVAL, _, _)) ->
            loop (Filename.dirname dir) rest ~symlinks_followed
          | target ->
            let symlinks_followed = symlinks_followed + 1 in
            if symlinks_followed = 1000 then raise Exit;
            (* The current dir is a symlink, we need to resolve it before we can interpret
               the ".." *)
            if Filename.is_relative target then
              loop dir (split_relative_dir target @ components) ~symlinks_followed
            else
              loop target components ~symlinks_followed
        else
          loop (dir ^/ component) rest ~symlinks_followed
    in
    let components = split_relative_dir relative_dir in
    try
      loop dir components ~symlinks_followed:0
    with Exit ->
      raise (Unix.Unix_error (ELOOP, "chdir", dir ^/ relative_dir))

  let chdir t dir =
    if dir = Filename.current_dir_name then
      t
    else
      let logical =
        if Filename.is_relative dir then
          chdir_logical t.logical dir
        else
          dir
      in
      let physical =
        Physical.map_unix t.physical ~f:(fun fd ->
          Posixat.openat ~dir:fd ~flags:[O_RDONLY] ~path:dir ~perm:0)
      in
      { logical; physical }

  let addref t = Physical.addref t.physical
  let deref t = Physical.deref t.physical
end

module Uenv = struct
  type t =
    { entries : string SMap.t
    ; (* The [PATH] variable pre-splitted *)
      path    : string list
    }

  let get t var = SMap.lookup var t.entries

  let split_path str =
    let len = String.length str in
    let is_sep c = c = ':' in
    let rec loop i j =
      if j = len then
        [String.sub str ~pos:i ~len:(j - i)]
      else
      if is_sep str.[j] then
        String.sub str ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
      else
        loop i (j + 1)
    in
    loop 0 0

  let set t var value =
    let entries = SMap.add t.entries ~key:var ~data:value in
    if var = "PATH" then
      { entries; path = split_path value }
    else
      { t with entries }

  let unset t var =
    let entries = SMap.remove var t.entries in
    if var = "PATH" then
      { entries; path = [] }
    else
      { t with entries }

  let create initial ~pwd =
    let entries =
      match initial with
      | Some l ->
        List.fold_left l ~init:SMap.empty ~f:(fun acc (key, data) ->
          SMap.add acc ~key ~data)
      | None ->
        Array.fold_left (Unix.environment ()) ~init:SMap.empty ~f:(fun acc s ->
          let i = String.index s '=' in
          let key = String.sub s ~pos:0 ~len:i in
          let data = String.sub s ~pos:(i + 1) ~len:(String.length s - i - 1) in
          SMap.add acc ~key ~data)
    in
    let entries = SMap.add entries ~key:"PWD" ~data:pwd in
    let path =
      match SMap.find "PATH" entries with
      | exception Not_found -> []
      | s -> split_path s
    in
    { entries; path }
end

type t =
  { stdin        : Unix.file_descr
  ; stdout       : Unix.file_descr
  ; stderr       : Unix.file_descr
  ; cwd          : Working_dir.t
  ; unix_env     : Uenv.t
  }

let create ?(stdin=Unix.stdin) ?(stdout=Unix.stdout) ?(stderr=Unix.stderr)
      ?(cwd=Working_dir_spec.Inherit) ?unix_env () =
  let cwd = Working_dir.create cwd in
  let unix_env = Uenv.create unix_env ~pwd:cwd.logical in
  { stdin
  ; stdout
  ; stderr
  ; cwd
  ; unix_env
  }

let add_cwd_ref t = Working_dir.addref t.cwd
let deref_cwd t = Working_dir.deref t.cwd

let get_env t var = Uenv.get t.unix_env var
let set_env t var value = { t with unix_env = Uenv.set t.unix_env var value }
let unset_env t var = { t with unix_env = Uenv.unset t.unix_env var }

let set_env_many t bindings =
  let unix_env =
    List.fold_left bindings ~init:t.unix_env ~f:(fun uenv (var, value) ->
      Uenv.set uenv var value)
  in
  { t with unix_env }

let unset_env_many t bindings =
  let unix_env =
    List.fold_left bindings ~init:t.unix_env ~f:Uenv.unset
  in
  { t with unix_env }

let stdin  t = t.stdin
let stdout t = t.stdout
let stderr t = t.stderr

let set_stdin  t fd = { t with stdin  = fd }
let set_stdout t fd = { t with stdout = fd }
let set_stderr t fd = { t with stderr = fd }

let set_outputs t fd = { t with stdout = fd; stderr = fd }

let set_stdios t ~stdin ~stdout ~stderr = { t with stdin; stdout; stderr }

let get_stdio t (which : Std_io.t) =
  match which with
  | Stdin  -> t.stdin
  | Stdout -> t.stdout
  | Stderr -> t.stderr

let set_stdio t (which : Std_io.t) x =
  match which with
  | Stdin  -> set_stdin  t x
  | Stdout -> set_stdout t x
  | Stderr -> set_stderr t x

let cwd_logical t = t.cwd.logical

let chdir t dir =
  let cwd = Working_dir.chdir t.cwd dir in
  let unix_env = Uenv.set t.unix_env "PWD" cwd.logical in
  { t with cwd; unix_env }

let find_executable t exe =
  let rec loop = function
    | [] -> None
    | path :: rest ->
      let fn = path ^/ exe in
      if Sys.file_exists fn then
        Some fn
      else
        loop rest
  in
  if not (Filename.is_relative exe) then
    Some exe
  else if Filename.basename exe <> exe then
    Some exe
  else
    loop t.unix_env.path

type run_error =
  | Command_not_found

let spawn t ~prog ~args =
  match find_executable t prog with
  | None -> Error Command_not_found
  | Some real_prog ->
    let env =
      SMap.fold t.unix_env.entries ~init:[] ~f:(fun ~key ~data acc ->
        sprintf "%s=%s" key data :: acc)
      |> Spawn.Env.of_list
    in
    let cwd : Spawn.Working_dir.t =
      match t.cwd.physical with
      | Win32 -> Path t.cwd.logical
      | Unix u -> Fd u.fd
    in
    let pid =
      Spawn.spawn ()
        ~env
        ~cwd
        ~prog:real_prog
        ~argv:(prog :: args)
        ~stdin:t.stdin
        ~stdout:t.stdout
        ~stderr:t.stderr
    in
    Ok pid

type full_path =
  | Path of string
  | In_dir of Unix.file_descr * string

let mk_logical_path t path =
  if Filename.is_relative path then
    t.cwd.logical ^/ path
  else
    path

let full_path t path =
  match t.cwd.physical with
  | Win32 ->
    Path (mk_logical_path t path)
  | Unix u ->
    In_dir (u.fd, path)

type full_path2 =
  | Path of string * string
  | In_dir of Unix.file_descr * string * string

let full_path2 t path1 path2 =
  match t.cwd.physical with
  | Win32 ->
    Path (mk_logical_path t path1, mk_logical_path t path2)
  | Unix u ->
    In_dir (u.fd, path1, path2)

let open_file t ?(perm=0) ~flags path =
  match full_path t path with
  | Path path ->
    Unix.openfile path (O_CLOEXEC :: flags) perm
  | In_dir (dir,  path) ->
    Posixat.openat ~dir ~path ~flags:(O_CLOEXEC :: flags) ~perm

let close_noerr t = try Unix.close t with _ -> ()

let with_file t ?perm ~flags path ~f =
  let fd = open_file t path ~flags ?perm in
  match f fd with
  | x           -> Unix.close fd; x
  | exception e -> close_noerr fd; raise e

let mkdir_one t ~perm path =
  match full_path t path with
  | Path path ->
    Unix.mkdir path perm
  | In_dir (dir,  path) ->
    Posixat.mkdirat ~dir ~path ~perm

let rec mkdir t ?(perm=0o777) ?(p=false) path =
  if not p then
    mkdir_one t path ~perm
  else
    try
      mkdir_one t path ~perm
    with
    | Unix.Unix_error((EEXIST|EISDIR) , _, _) -> ()
    | Unix.Unix_error(ENOENT, _, _) when Filename.basename path <> path ->
      mkdir t (Filename.dirname path) ~perm ~p;
      mkdir_one t path ~perm

let chmod t path ~perm =
  match full_path t path with
  | Path path ->
    Unix.chmod path perm
  | In_dir (dir, path) ->
    Posixat.fchmodat ~dir ~path ~perm ~flags:[]

let chown t path ~uid ~gid =
  match full_path t path with
  | Path path ->
    Unix.chown path uid gid
  | In_dir (dir, path) ->
    Posixat.fchownat ~dir ~path ~uid ~gid ~flags:[]

let rm t path =
  match full_path t path with
  | Path path ->
    Unix.unlink path
  | In_dir (dir,  path) ->
    Posixat.unlinkat ~dir ~path ~flags:[]

let rmdir t path =
  match full_path t path with
  | Path path ->
    Unix.rmdir path
  | In_dir (dir,  path) ->
    Posixat.unlinkat ~dir ~path ~flags:[AT_REMOVEDIR]

let mkfifo t ?(perm=0o666) path =
  match full_path t path with
  | Path path ->
    Unix.mkfifo path perm
  | In_dir (dir,  path) ->
    Posixat.mkfifoat ~dir ~path ~perm

let link t oldpath newpath =
  match full_path2 t oldpath newpath with
  | Path (oldpath, newpath) ->
    Unix.link oldpath newpath
  | In_dir (dir,  oldpath, newpath) ->
    Posixat.linkat ~olddir:dir ~newdir:dir ~oldpath ~newpath ~flags:[]

let symlink t oldpath newpath =
  match full_path2 t oldpath newpath with
  | Path (oldpath, newpath) ->
    Unix.symlink oldpath newpath
  | In_dir (dir,  oldpath, newpath) ->
    Posixat.symlinkat ~newdir:dir ~oldpath ~newpath

let rename t oldpath newpath =
  match full_path2 t oldpath newpath with
  | Path (oldpath, newpath) ->
    Unix.rename oldpath newpath
  | In_dir (dir,  oldpath, newpath) ->
    Posixat.renameat ~olddir:dir ~newdir:dir ~oldpath ~newpath

let stat t path =
  match full_path t path with
  | Path path ->
    Unix.stat path
  | In_dir (dir,  path) ->
    Posixat.fstatat ~dir ~path ~flags:[]

let lstat t path =
  match full_path t path with
  | Path path ->
    Unix.lstat path
  | In_dir (dir,  path) ->
    Posixat.fstatat ~dir ~path ~flags:[AT_SYMLINK_NOFOLLOW]

let readlink t path =
  match full_path t path with
  | Path path ->
    Unix.readlink path
  | In_dir (dir,  path) ->
    Posixat.readlinkat ~dir ~path

let access t path mode =
  match full_path t path with
  | Path path ->
    Unix.access path mode
  | In_dir (dir,  path) ->
    Posixat.faccessat ~dir ~path ~mode ~flags:[]

let readdir t path =
  match full_path t path with
  | Path path ->
    Sys.readdir path |> Array.to_list
  | In_dir (dir,  path) ->
    let fd = Posixat.openat ~dir ~path ~flags:[O_RDONLY] ~perm:0 in
    match Posixat.fdopendir fd with
    | exception e -> close_noerr fd; raise e
    | dh ->
      let rec loop dh acc =
        match Unix.readdir dh with
        | "." | ".." -> loop dh acc
        | fname -> loop dh (fname :: acc)
        | exception End_of_file -> Unix.closedir dh; List.rev acc
        | exception e ->
          Unix.closedir dh;
          raise e
      in
      loop dh []
