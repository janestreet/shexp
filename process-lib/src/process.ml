open Import

let with_lock mutex ~f =
  Mutex.lock mutex;
  protectx () ~finally:(fun () -> Mutex.unlock mutex) ~f

module Context = struct
  module State = struct
    type t =
      | Active of Env.t
      | Disposed
  end

  type t =
    { mutable state : State.t
    ; mutable mutex : Mutex.t
    }

  module Working_dir = Env.Working_dir_spec

  let create ?stdin ?stdout ?stderr ?cwd ?unix_env () =
    { state = Active (Env.create ?stdin ?stdout ?stderr ?cwd ?unix_env ())
    ; mutex = Mutex.create ()
    }

  let dispose t =
    match
      with_lock t.mutex ~f:(fun () ->
        match t.state with
        | Disposed -> None
        | Active env -> t.state <- Disposed; Some env)
    with
    | None -> ()
    | Some env -> Env.deref_cwd env

  let use_env t =
    with_lock t.mutex ~f:(fun () ->
      match t.state with
      | Disposed -> failwith "Shexp_process: \
                              trying to use a disposed context"
      | Active env -> Env.add_cwd_ref env; env)
end

module Temp0 = struct
  (* Copied from filename.ml and adapted to pass O_CLOEXEC as well as protecting the
     global random state *)

  let prng = lazy(Random.State.make_self_init ())
  let prng_mutex = Mutex.create ()

  let gen_name ~temp_dir ~prefix ~suffix =
    let rnd =
      with_lock prng_mutex ~f:(fun () ->
        (Random.State.bits (Lazy.force prng)) land 0xFFFFFF)
    in
    temp_dir ^/ (Printf.sprintf "%s%06x%s" prefix rnd suffix)

  let create ~temp_dir ~prefix ~suffix ~mk =
    let rec try_name counter =
      let name = gen_name ~temp_dir ~prefix ~suffix in
      match mk name with
      | x -> (x, name)
      | exception (Unix.Unix_error _) when counter < 1000 ->
        try_name (counter + 1)
    in
    try_name 0

  let with_open_temp ~prefix ~suffix ~f =
    let finally (fd, name) =
      (try Unix.close  fd   with _ -> ());
      (try Unix.unlink name with _ -> ())
    in
    protectx ~finally ~f
      (create ~temp_dir:(Filename.get_temp_dir_name ()) ~prefix ~suffix
         ~mk:(fun fn -> Unix.openfile fn [O_WRONLY; O_CREAT; O_EXCL] 0o600))
end

type 'a t =
  | Return of 'a
  | Error of { exn : exn; backtrace : Printexc.raw_backtrace }
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Protect of
      { finally : unit t
      ; t       : 'a t
      }
  (* Note: it is expected that the function in the following two constructors never
     raise *)
  | Env_get :
      { prim : ('a, 'b) Prim.t
      ; args : ('a, 'b) Prim.Args.t
      } -> 'b t
  | Env_set :
      { prim : ('a, Env.t) Prim.t
      ; args : ('a, Env.t) Prim.Args.t
      ; k    : 'b t
      } -> 'b t
  | Fork : 'a t * 'b t -> ('a * 'b) t
  | Prim :
      { prim : ('a, 'b) Prim.t
      ; args : ('a, 'b) Prim.Args.t
      } -> 'b t
  | Chdir of
      { dir : string
      ; k   : 'a t
      }
  | Fold :
      { fold : 'a. Env.t -> init:'a -> f:('a -> 'elt -> 'a) -> 'a
      ; init : 'acc
      ; f    : 'acc -> 'elt -> 'acc t
      ; prim : ('a, 'elt option) Prim.t
      ; args : ('a, 'elt option) Prim.Args.t
      } -> 'acc t

let rec might_block : type a. a t -> bool = function
  | Bind _ | Prim _ | Chdir _ | Fold _ -> true
  | Return _ | Error _ | Env_get _ -> false
  | Env_set  { k; _ } -> might_block k
  | Fork (a, b) -> might_block a || might_block b
  | Protect { t; finally } ->  might_block t || might_block finally

let deref_cwd env ~can_deref_cwd =
  if can_deref_cwd then
    Env.deref_cwd env
  else
    ()

let rec exec : type a. Env.t -> can_deref_cwd:bool -> a t -> a
  = fun env ~can_deref_cwd t ->
    match t with
    | Return x ->
      deref_cwd env ~can_deref_cwd;
      x

    | Error { exn; _ } ->
      deref_cwd env ~can_deref_cwd;
      reraise exn

    | Env_get { prim; args } ->
      deref_cwd env ~can_deref_cwd;
      Prim.run prim env args

    | Env_set { prim; args; k } ->
      let env = Prim.run prim env args in
      exec env k ~can_deref_cwd

    | Prim { prim; args } ->
      if can_deref_cwd then
        match Prim.run prim env args with
        | x ->
          Env.deref_cwd env;
          x
        | exception exn ->
          Env.deref_cwd env;
          reraise exn
      else
        Prim.run prim env args

    | Bind (t, f) ->
      if can_deref_cwd then
        match f (exec env t ~can_deref_cwd:false) with
        | exception exn ->
          Env.deref_cwd env;
          reraise exn
        | t ->
          exec env t ~can_deref_cwd:true
      else
        let t = f (exec env t ~can_deref_cwd:false) in
        exec env t ~can_deref_cwd:false

    | Protect { finally; t } ->
      (match exec env t ~can_deref_cwd:false  with
       | x ->
         exec env finally ~can_deref_cwd;
         x
       | exception exn ->
         exec env finally ~can_deref_cwd;
         reraise exn)

    | Chdir { dir; k } ->
      if can_deref_cwd then
        match Env.chdir env dir with
        | new_env ->
          Env.deref_cwd env;
          exec new_env k ~can_deref_cwd:true
        | exception exn ->
          Env.deref_cwd env;
          reraise exn
      else
        let new_env = Env.chdir env dir in
        exec new_env k ~can_deref_cwd:true

    | Fold { fold; f; init; _ } ->
      if can_deref_cwd then
        match
          fold env ~init ~f:(fun acc elt ->
            exec env (f acc elt) ~can_deref_cwd:false)
        with
        | res ->
          Env.deref_cwd env;
          res
        | exception exn ->
          Env.deref_cwd env;
          reraise exn
      else
        fold env ~init ~f:(fun acc elt ->
          exec env (f acc elt) ~can_deref_cwd:false)

    | Fork (a, b) ->
      if might_block a && might_block b then begin
        if can_deref_cwd then Env.add_cwd_ref env;
        let job = Job.detach ~f:(fun () -> exec env b ~can_deref_cwd) in
        let a_res = exec env a ~can_deref_cwd in
        let b_res = Job.wait job in
        (a_res, b_res)
      end else if can_deref_cwd then
        match exec env a ~can_deref_cwd:false with
        | exception exn ->
          Env.deref_cwd env;
          reraise exn
        | a_res ->
          (a_res, exec env b ~can_deref_cwd:true)
      else
        let a_res = exec env a ~can_deref_cwd:false in
        (a_res, exec env b ~can_deref_cwd:false)

let eval ?context t =
  let env =
    match context with
    | None -> Env.create ()
    | Some ctx -> Context.use_env ctx
  in
  exec env t ~can_deref_cwd:true

module Prim = Prim
module type Debugger = Debugger_intf.S

module With_debug(D : Debugger) = struct
  let chdir_prim =
    Prim.make "chdir" [A sexp_of_string] Env (fun _ _ -> assert false)

  module D = struct
    type checkpoint =
      | Not_needed
      | Create_if_needed
      | Created

    type capture_context =
      { mutable pos : int
      ; fdw         : Unix.file_descr
      ; fdr         : Unix.file_descr
      }

    type t =
      { dbg                : D.t
      ; capture            : capture_context option
      ; mutable checkpoint : checkpoint
      }

    let need_checkpoint t =
      if t.checkpoint = Create_if_needed then begin
        D.enter_sub t.dbg;
        t.checkpoint <- Created;
      end

    let before_prim t prim args = D.before_prim t.dbg prim args
    let after_prim t prim res tok = D.after_prim t.dbg prim res tok
    let user_exn t exn bt = D.user_exn t.dbg exn bt
    let force_threads = D.force_threads

    let capture t =
      match t.capture with
      | None -> ()
      | Some cap ->
        let pos = Unix.lseek cap.fdw 0 SEEK_CUR in
        let len = pos - cap.pos in
        if len > 0 then begin
          let s = Bigstring.read_exactly cap.fdr len in
          cap.pos <- pos;
          D.output t.dbg s
        end

    let enter_new_thread_with_capture env dbg ~parent_capture ~f =
      let replace_stdin, replace_stdout, replace_stderr =
        match parent_capture with
        | None -> (false, true, true)
        | Some info ->
          ( Env.stdin  env == info.fdw
          , Env.stdout env == info.fdw
          , Env.stderr env == info.fdw
          )
      in
      if not (replace_stdin || replace_stdout || replace_stderr) then
        f env { dbg
              ; capture    = None
              ; checkpoint = Not_needed
              }
      else
        Temp0.with_open_temp ~prefix:"shexp-process" ~suffix:".output"
          ~f:(fun (fdw, fn) ->
            protectx (Unix.openfile fn [O_RDONLY] 0)
              ~finally:(fun fd -> try Unix.close fd with _ -> ())
              ~f:(fun fdr ->
                let t = { dbg
                        ; capture    = Some { pos = 0; fdr; fdw }
                        ; checkpoint = Not_needed
                        }
                in
                let env =
                  Env.set_stdios env
                    ~stdin: (if replace_stdin  then fdw else Env.stdin  env)
                    ~stdout:(if replace_stdout then fdw else Env.stdout env)
                    ~stderr:(if replace_stderr then fdw else Env.stderr env)
                in
                let res = f env t in
                capture t;
                res))

    let fork env t ~f =
      protectx (D.fork t.dbg)
        ~finally:(fun (dbg_a, dbg_b) -> D.end_fork t.dbg dbg_a dbg_b)
        ~f:(fun (dbg_a, dbg_b) ->
          match t.capture with
          | None ->
            f
              env { t with dbg = dbg_a; checkpoint = Not_needed }
              env { t with dbg = dbg_b; checkpoint = Not_needed }
          | Some _ ->
            enter_new_thread_with_capture env dbg_b ~parent_capture:t.capture
              ~f:(fun env' t' ->
                f env { t with dbg = dbg_a; checkpoint = Not_needed } env' t'))

    let toplevel env dbg ~capture ~f =
      if capture then
        enter_new_thread_with_capture env dbg ~parent_capture:None ~f
      else
        f env { dbg; capture = None; checkpoint = Not_needed }

    let sub t ~f =
      protectx { t with checkpoint = Create_if_needed } ~f ~finally:(fun t ->
        if t.checkpoint = Created then D.leave_sub t.dbg)
  end

  let to_result f =
    match f () with
    | x           -> Ok x
    | exception e -> Error (e, Printexc.get_raw_backtrace ())

  let ok_exn = function
    | Ok x -> x
    | Error (e, _) -> raise e

  let rec exec : type a. Env.t
    -> D.t
    -> can_deref_cwd:bool
    -> a t
    -> a
    = fun env dbg ~can_deref_cwd t ->
      match t with
      | Return x ->
        deref_cwd env ~can_deref_cwd;
        x

      | Error { exn; backtrace } ->
        deref_cwd env ~can_deref_cwd;
        D.user_exn dbg exn backtrace;
        raise exn

      | Env_get { prim; args } ->
        deref_cwd env ~can_deref_cwd;
        let token = D.before_prim dbg prim args in
        let res = Prim.run prim env args in
        D.after_prim dbg prim (Ok res) token;
        res

      | Env_set { prim; args; k } ->
        D.need_checkpoint dbg;
        let token = D.before_prim dbg prim args in
        let env = Prim.run prim env args in
        D.after_prim dbg prim (Ok env) token;
        exec env dbg k ~can_deref_cwd

      | Prim { prim; args } ->
        let token = D.before_prim dbg prim args in
        let res = to_result (fun () -> Prim.run prim env args) in
        deref_cwd env ~can_deref_cwd;
        D.capture dbg;
        D.after_prim dbg prim res token;
        ok_exn res

      | Bind (t, f) ->
        (match exec_sub env dbg t with
         | exception exn ->
           deref_cwd env ~can_deref_cwd;
           reraise exn
         | x ->
           match f x with
           | exception exn ->
             let backtrace = Printexc.get_raw_backtrace () in
             deref_cwd env ~can_deref_cwd;
             D.user_exn dbg exn backtrace;
             reraise exn
           | t ->
             exec env dbg t ~can_deref_cwd)

      | Protect { finally; t } ->
        (match exec_sub env dbg t with
         | x ->
           exec env dbg finally ~can_deref_cwd;
           x
         | exception exn ->
           exec env dbg finally ~can_deref_cwd;
           reraise exn)

      | Chdir { dir; k } ->
        D.need_checkpoint dbg;
        let token = D.before_prim dbg chdir_prim (A1 dir) in
        let res = to_result (fun () -> Env.chdir env dir) in
        deref_cwd env ~can_deref_cwd;
        D.after_prim dbg chdir_prim res token;
        exec (ok_exn res) dbg k ~can_deref_cwd:true

      | Fold { fold; f; init; prim; args } ->
        let token = ref (D.before_prim dbg prim args) in
        let in_fold_impl = ref true in
        let res =
          to_result (fun () ->
            fold env ~init ~f:(fun acc elt ->
              in_fold_impl := false;
              D.capture dbg;
              D.after_prim dbg prim (Ok (Some elt)) !token;
              match f acc elt with
              | exception exn ->
                D.user_exn dbg exn (Printexc.get_raw_backtrace ());
                reraise exn
              | t ->
                let acc = exec env dbg t ~can_deref_cwd:false in
                token := D.before_prim dbg prim args;
                in_fold_impl := true;
                acc))
        in
        deref_cwd env ~can_deref_cwd;
        if !in_fold_impl then begin
          D.capture dbg;
          match res with
          | Ok acc ->
            D.after_prim dbg prim (Ok None) !token;
            acc
          | Error (exn, _) as e ->
            D.after_prim dbg prim e !token;
            reraise exn
        end else begin
          match res with
          | Ok _ -> assert false
          | Error (exn, _) -> reraise exn
        end

      | Fork (a, b) ->
        D.fork env dbg ~f:(fun env_a dbg_a env_b dbg_b ->
          if D.force_threads || (might_block a && might_block b) then begin
            if can_deref_cwd then Env.add_cwd_ref env_a;
            let job = Job.detach ~f:(fun () -> exec env_b dbg_b b ~can_deref_cwd) in
            let a_res = to_result (fun () -> exec env_a dbg_a a ~can_deref_cwd) in
            let b_res = Job.wait job in
            match a_res with
            | Ok a_res -> (a_res, b_res)
            | Error (exn, _) -> reraise exn
          end else
            let a_res = to_result (fun () -> exec env dbg_a a ~can_deref_cwd:false) in
            let b_res = exec env dbg_b b ~can_deref_cwd in
            match a_res with
            | Ok a_res -> (a_res, b_res)
            | Error (exn, _) -> reraise exn)

  and exec_sub : type a. Env.t -> D.t -> a t -> a
    = fun env dbg t ->
      D.sub dbg ~f:(fun dbg -> exec env dbg t ~can_deref_cwd:false)

  let eval ?context ?(capture=false) t ~debugger =
    let env =
      match context with
      | None -> Env.create ()
      | Some ctx -> Context.use_env ctx
    in
    D.toplevel env debugger ~capture ~f:(fun env dbg ->
      exec env dbg t ~can_deref_cwd:true)
end

module Logged = struct
  open Debuggers

  module M = With_debug(Logger)

  let log_default sexp =
    prerr_endline (Sexp.to_string sexp)

  let eval ?context ?capture ?(log=log_default) t =
    M.eval ?context ?capture t ~debugger:(Logger.create log)
end

module Traced = struct
  open Debuggers

  module M = With_debug(Tracer)

  let eval ?context ?capture t =
    let dbg = Tracer.create () in
    let res =
      match M.eval ?context ?capture t ~debugger:dbg with
      | x             -> Ok x
      | exception exn -> Error exn
    in
    (res, Tracer.result dbg)

  let eval_exn ?context ?capture t =
    let dbg = Tracer.create () in
    let res = M.eval ?context ?capture t ~debugger:dbg in
    (res, Tracer.result dbg)
end

let return x = Return x
let bind t ~f = Bind (t, f)
let map t ~f = Bind (t, fun x -> Return (f x))
let fork a b = Fork (a, b)
let fork_unit a b = map (fork a b) ~f:(fun (x, ()) -> x)
let protect ~finally t = Protect { finally; t }
let reify_exn f x =
  match f x with
  | t -> t
  | exception exn -> Error { exn; backtrace = Printexc.get_raw_backtrace () }
let fail exn = Error { exn; backtrace = Printexc.get_raw_backtrace () }

module Infix0 = struct
  let ( >>= ) t f = bind t ~f
  let ( >>| ) t f = map t ~f
  let ( >> ) a b = a >>= fun () -> b
end
open Infix0

module List0 = struct
  let rec iter l ~f =
    match l with
    | [] -> return ()
    | x :: l -> f x >>= fun () -> iter l ~f
end

let quote_for_errors s =
  let need_quoting = ref false in
  String.iter s ~f:(function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '=' | '+' | '_' | '/' -> ()
    | _ -> need_quoting := true);
  if !need_quoting then
    Filename.quote s
  else
    s

let cmd_line prog args =
  List.map (prog :: args) ~f:quote_for_errors |> String.concat ~sep:" "

let pack0 prim = Prim { prim; args = A0 () }
let pack1 prim a = Prim { prim; args = A1 a }
let pack2 prim a b = Prim { prim; args = A2 (a, b) }
let pack3 prim a b c = Prim { prim; args = A3 (a, b, c) }

module Exit_status = Exit_status

let waitpid pid : Exit_status.t =
  match snd (Unix.waitpid [] pid) with
  | WEXITED   n -> Exited n
  | WSIGNALED n -> Signaled n
  | WSTOPPED  _ -> assert false

module Background_command = struct
  type t =
    { mutex        : Mutex.t
    ; pid          : int
    ; mutable wait : Exit_status.t Lazy.t
    }

  let sexp_of_t t = sexp_of_string (Printf.sprintf "[%d]" t.pid)

  let create pid =
    { mutex = Mutex.create ()
    ; pid
    ; wait = lazy (waitpid pid)
    }

  let pid t = t.pid

  let wait t =
    Mutex.lock t.mutex;
    protectx t.mutex ~finally:Mutex.unlock ~f:(fun _ ->
      Lazy.force t.wait)
end

let spawn =
  let prim =
    Prim.make "spawn"
      [ A sexp_of_string
      ; A (sexp_of_list sexp_of_string)
      ]
      (F Background_command.sexp_of_t)
      (fun env prog args ->
         match Env.spawn env ~prog ~args with
         | Ok pid -> Background_command.create pid
         | Error Command_not_found ->
           Printf.ksprintf failwith "%s: command not found" (quote_for_errors prog))
  in
  fun prog args -> pack2 prim prog args

let wait =
  let prim =
    Prim.make "wait"
      [ A Background_command.sexp_of_t
      ]
      (F Exit_status.sexp_of_t)
      (fun _ bc -> Background_command.wait bc)
  in
  fun bc -> pack1 prim bc

(* This could be implemented in term of [spawn] followed by a [wait], but doing it in one
   primitive improve traces. *)
let run_exit_status =
  let prim =
    Prim.make "run"
      [ A sexp_of_string
      ; A (sexp_of_list sexp_of_string)
      ]
      (F Exit_status.sexp_of_t)
      (fun env prog args ->
         match Env.spawn env ~prog ~args with
         | Ok pid -> waitpid pid
         | Error Command_not_found ->
           Printf.ksprintf failwith "%s: command not found" (quote_for_errors prog))
  in
  fun prog args -> pack2 prim prog args

let run_exit_code prog args =
  run_exit_status prog args >>| function
  | Exited n -> n
  | Signaled signal ->
    Printf.ksprintf failwith "Command got signal %d: %s"
      signal (cmd_line prog args)

let run prog args =
  run_exit_code prog args >>| fun code ->
  if code <> 0 then
    Printf.ksprintf failwith "Command exited with code %d: %s"
      code (cmd_line prog args)

let run_bool ?(true_v=[0]) ?(false_v=[1]) prog args =
  run_exit_code prog args >>| fun code ->
  if List.mem code ~set:true_v then
    true
  else if List.mem code ~set:false_v then
    false
  else
    Printf.ksprintf failwith "Command exited with unexpected code %d: %s"
      code (cmd_line prog args)

let call_exit_status = function
  | [] -> failwith "call_exit_status: empty command"
  | prog::args -> run_exit_status prog args

let call_exit_code = function
  | [] -> failwith "call_exit_code: empty command"
  | prog::args -> run_exit_code prog args

let call = function
  | [] -> failwith "call: empty command"
  | prog::args -> run prog args

let call_bool ?true_v ?false_v = function
  | [] -> failwith "call_bool: empty command"
  | prog::args -> run_bool ?true_v ?false_v prog args

let find_executable =
  let prim =
    Prim.make "find-executable"
      [A sexp_of_string]
      (F (sexp_of_option sexp_of_string))
      Env.find_executable
  in
  fun exe -> pack1 prim exe

let find_executable_exn exe =
  find_executable exe >>| function
  | None -> Printf.ksprintf failwith "command %S not found" exe
  | Some x -> x

let get_env =
  let prim =
    Prim.make "get-env"
      [A sexp_of_string]
      (F (sexp_of_option sexp_of_string))
      Env.get_env
  in
  fun var -> Env_get { prim; args = A1 var }

let get_env_exn var =
  get_env var >>| function
  | None -> Printf.ksprintf failwith "environment variable %S not found" var
  | Some x -> x

let set_env =
  let prim =
    { Prim.
      name   = "set-env"
    ; args   = [A sexp_of_string; A sexp_of_string]
    ; result = Env
    ; run    = Env.set_env
    }
  in
  fun var value k -> Env_set { k; prim; args = A2 (var, value) }

let unset_env =
  let prim =
    { Prim.
      name   = "unset-env"
    ; args   = [A sexp_of_string]
    ; result = Env
    ; run    = Env.unset_env
    }
  in
  fun var k -> Env_set { k; prim; args = A1 var }

let cwd_logical =
  let prim =
    Prim.make "cwd" [] (F sexp_of_string) Env.cwd_logical
  in
  Env_get { prim; args = A0 () }

let chdir dir k = Chdir { dir; k }

module Std_io = Std_io

let echo =
  let prim =
    Prim.make "echo"
      [ O ("where", Std_io.sexp_of_t, Stdout)
      ; O ("n", sexp_of_bool, false)
      ; A sexp_of_string
      ]
      Unit
      (fun env where n str ->
         let str = if not n then str ^ "\n" else str in
         Bigstring.write_all (Env.get_stdio env where) str)
  in
  fun ?(where=Std_io.Stdout) ?n str ->
    pack3 prim where (n <> None) str

let  print str = echo str ~n:() ~where:Stdout
let eprint str = echo str ~n:() ~where:Stderr
let  printf fmt = Printf.ksprintf  print fmt
let eprintf fmt = Printf.ksprintf eprint fmt

let read_all =
  let prim =
    Prim.make "read-all" [] (F sexp_of_string)
      (fun env -> Bigstring.read_all (Env.stdin env))
  in
  pack0 prim


let fold_lines : type a. init:a -> f:(a -> string -> a t) -> a t =
  let prim =
    { Prim.
      name   = "read-line"
    ; args   = []
    ; result = F (fun x -> sexp_of_option sexp_of_string x)
    ; run    = fun _ -> assert false
    }
  in
  fun ~init ~f ->
    Fold { prim
         ; args = A0 ()
         ; init
         ; f
         ; fold = fun env ~init ~f ->
             Bigstring.fold_lines (Env.stdin env) ~init ~f
         }

let fold_chunks =
  let prim =
    { Prim.
      name   = "read-chunk"
    ; args   = [L ("sep", sexp_of_char)]
    ; result = F (fun x -> sexp_of_option sexp_of_string x)
    ; run    = fun _ -> assert false
    }
  in
  fun ~sep ~init ~f ->
    Fold { prim
         ; args = A1 sep
         ; init
         ; f
         ; fold = fun env ~init ~f ->
             Bigstring.fold_chunks (Env.stdin env) ~sep ~init ~f
         }

let iter_lines f = fold_lines ~init:() ~f:(fun () line -> f line)
let iter_chunks ~sep f = fold_chunks ~sep ~init:() ~f:(fun () line -> f line)

let create_pipe =
  let prim =
    Prim.make "create-pipe" [] (F (Sexp.pair Posixat.Fd.sexp_of_t Posixat.Fd.sexp_of_t))
      (fun _ -> retry_eintr1 W.(pair fd fd) Spawn.safe_pipe ())
  in
  pack0 prim

let close_fd =
  let prim =
    Prim.make "close-fd" [A Posixat.Fd.sexp_of_t] Unit
      (fun _ fd -> Unix.close fd)
  in
  fun fd -> pack1 prim fd

let set_ios =
  let prim =
    { Prim.
      name   = "set-ios"
    ; args   = [A (fun x -> sexp_of_list Std_io.sexp_of_t x); A Posixat.Fd.sexp_of_t]
    ; result = Env
    ; run    = fun env ios fd ->
      List.fold_left ios ~init:env ~f:(fun env where ->
        Env.set_stdio env where fd)
    }
  in
  fun ios fd k -> Env_set { k; prim; args = A2 (ios, fd) }

let pipe_both ?(connect=Std_io.([Stdout],Stdin)) a b =
  create_pipe >>= fun (fdr, fdw) ->
  let (aios, bio) = connect in
  fork
    (protect ~finally:(close_fd fdw) (set_ios aios fdw a))
    (protect ~finally:(close_fd fdr) (set_ios [bio] fdr b))

let pipe ?connect a b = pipe_both ?connect a b >>| snd

let epipe      a b = pipe      ~connect:([Stderr], Stdin) a b
let epipe_both a b = pipe_both ~connect:([Stderr], Stdin) a b

let capture      ios t = pipe_both ~connect:(ios, Stdin) t read_all
let capture_unit ios t = pipe      ~connect:(ios, Stdin) t read_all

let open_file =
  let prim =
    Prim.make "open-file"
      [ O ("perm",  Posixat.File_perm.sexp_of_t, 0)
      ; L ("flags", sexp_of_list Posixat.Open_flag.sexp_of_t)
      ; A sexp_of_string
      ]
      (F Posixat.Fd.sexp_of_t)
      (fun env perm flags fn -> Env.open_file env ~perm ~flags fn)
  in
  fun ?(perm=0) ~flags fn ->
    pack3 prim perm flags fn

let redirect ios ?perm ~flags fn t =
  open_file ?perm ~flags fn >>= fun fd ->
  protect ~finally:(close_fd fd) (set_ios ios fd t)

let std_to ios ?append fn t =
  redirect ios ~flags:[O_WRONLY; O_CREAT; if append = None then O_TRUNC else O_APPEND]
    ~perm:0o666
    fn t

let stdout_to ?append  fn t = std_to ?append [Stdout] fn t
let stderr_to ?append  fn t = std_to ?append [Stderr] fn t
let outputs_to ?append fn t = std_to ?append [Stdout; Stderr] fn t

let stdin_from fn t = redirect [Stdin] ~flags:[O_RDONLY] ~perm:0 fn t

let replace_io =
  let prim =
    { Prim.
      name   = "replace-io"
    ; args   = [ O ("stdin" , Std_io.sexp_of_t, Stdin )
               ; O ("stdout", Std_io.sexp_of_t, Stdout)
               ; O ("stderr", Std_io.sexp_of_t, Stderr)
               ]
    ; result = Env
    ; run    = fun env stdin stdout stderr ->
      Env.set_stdios env
        ~stdin: (Env.get_stdio env stdin)
        ~stdout:(Env.get_stdio env stdout)
        ~stderr:(Env.get_stdio env stderr)
    }
  in
  fun ?(stdin=Std_io.Stdin) ?(stdout=Std_io.Stdout) ?(stderr=Std_io.Stderr) k ->
    Env_set { k; prim; args = A3 (stdin, stdout, stderr) }

let out_to_err t = replace_io ~stdout:Stderr t
let err_to_out t = replace_io ~stderr:Stdout t

let chmod =
  let prim =
    Prim.make "chmod"
      [ A sexp_of_string
      ; L ("perm", Posixat.File_perm.sexp_of_t)
      ]
      Unit
      (fun env fn perm -> Env.chmod env fn ~perm)
  in
  fun path ~perm -> pack2 prim path perm

let chown =
  let prim =
    Prim.make "chown"
      [ A sexp_of_string
      ; L ("uid", sexp_of_int)
      ; L ("gid", sexp_of_int)
      ]
      Unit
      (fun env fn uid gid -> Env.chown env fn ~uid ~gid)
  in
  fun path ~uid ~gid -> pack3 prim path uid gid

let mkdir =
  let prim =
    Prim.make "mkdir"
      [ O ("perm", Posixat.File_perm.sexp_of_t, 0o777)
      ; O ("p", sexp_of_bool, false)
      ; A sexp_of_string
      ]
      Unit
      (fun env perm p path -> Env.mkdir env path ~perm ~p)
  in
  fun ?(perm=0o777) ?p path -> pack3 prim perm (p <> None) path

let rm =
  let prim =
    Prim.make "rm"
      [ A sexp_of_string ]
      Unit
      Env.rm
  in
  fun fn -> pack1 prim fn

let rmdir =
  let prim =
    Prim.make "rmdir"
      [ A sexp_of_string ]
      Unit
      Env.rmdir
  in
  fun fn -> pack1 prim fn

let mkfifo =
  if Sys.win32 || Posixat.has_mkfifoat then
    let prim =
      Prim.make "mkfifo"
        [ O ("perm", Posixat.File_perm.sexp_of_t, 0o666)
        ; A sexp_of_string
        ]
        Unit
        (fun env perm path -> Env.mkfifo env path ~perm)
    in
    fun ?(perm=0o666) path -> pack2 prim perm path
  else
    fun ?(perm=0o666) path ->
      run "/usr/bin/mkfifo" ["-m"; Printf.sprintf "0o%3o" perm; "--"; path]

let link =
  let prim =
    Prim.make "link"
      [ A sexp_of_string
      ; A sexp_of_string
      ]
      Unit
      Env.link
  in
  fun oldpath newpath -> pack2 prim oldpath newpath

let rename =
  let prim =
    Prim.make "rename"
      [ A sexp_of_string
      ; A sexp_of_string
      ]
      Unit
      Env.rename
  in
  fun oldpath newpath -> pack2 prim oldpath newpath

let symlink =
  let prim =
    Prim.make "symlink"
      [ A sexp_of_string
      ; A sexp_of_string
      ]
      Unit
      Env.symlink
  in
  fun oldpath newpath -> pack2 prim oldpath newpath

let stat =
  let prim =
    Prim.make "stat"
      [ A sexp_of_string ]
      (F Posixat.Stats.sexp_of_t)
      Env.stat
  in
  fun path -> pack1 prim path

let lstat =
  let prim =
    Prim.make "lstat"
      [ A sexp_of_string ]
      (F Posixat.Stats.sexp_of_t)
      Env.lstat
  in
  fun path -> pack1 prim path

let readlink =
  let prim =
    Prim.make "readlink"
      [ A sexp_of_string ]
      (F sexp_of_string)
      Env.readlink
  in
  fun path -> pack1 prim path

let readdir =
  let prim =
    Prim.make "readdir"
      [ A sexp_of_string ]
      (F (sexp_of_list sexp_of_string))
      Env.readdir
  in
  fun path -> pack1 prim path

let file_exists =
  let prim =
    Prim.make "file-exists"
      [ A sexp_of_string ]
      (F sexp_of_bool)
      (fun env path ->
         match Env.access env path [F_OK] with
         | () -> true
         | exception (Unix.Unix_error (ENOENT, _, _)) -> false)
  in
  fun path -> pack1 prim path

let rec rm_rf path =
  readdir path >>= fun l ->
  List0.iter l ~f:(fun fname ->
    (* readdir does not give us "." and ".." *)
    let fpath = path ^/ fname in
    lstat fpath >>= fun stat ->
    match stat.st_kind with
    | S_DIR ->
      rm_rf fpath
    | _ ->
      rm fpath)
  >>= fun () ->
  rmdir path

let temp_dir_var = if Sys.win32 then "TEMP" else "TMPDIR"
let temp_dir_default = if Sys.win32 then "." else "/tmp"

let get_temp_dir env =
  match Env.get_env env temp_dir_var with
  | None -> temp_dir_default
  | Some d -> d

let temp_dir =
  let prim =
    Prim.make "temp-dir"
      []
      (F sexp_of_string)
      get_temp_dir
  in
  pack0 prim

let set_temp_dir =
  let prim =
    { Prim.
      name   = "set-temp-dir"
    ; args   = [A sexp_of_string]
    ; result = Env
    ; run    = fun env dir -> Env.set_env env temp_dir_var dir
    }
  in
  fun dir k -> Env_set { k; prim; args = A1 dir }

module Temp = struct
  let create what mk =
    let prim =
      Prim.make ("generate-temporary-" ^ what)
        [ L ("prefix", sexp_of_string)
        ; L ("suffix", sexp_of_string)
        ]
        (F sexp_of_string)
        (fun env prefix suffix ->
           let temp_dir = get_temp_dir env in
           let (), name =
             Temp0.create ~temp_dir ~prefix ~suffix ~mk:(fun fn ->
               mk env fn)
           in
           name)
    in
    fun ~prefix ~suffix -> pack2 prim prefix suffix

  let create_file =
    create "file" (fun env path ->
      Env.open_file env path ~perm:0o600 ~flags:[O_WRONLY; O_CREAT; O_EXCL]
      |> Unix.close)

  let create_dir =
    create "directory" (fun env path -> Env.mkdir env path ~perm:0o700)
end

let with_temp_file ~prefix ~suffix f =
  Temp.create_file ~prefix ~suffix >>= fun fn ->
  protect ~finally:(rm fn) (reify_exn f fn)

let with_temp_dir ~prefix ~suffix f =
  Temp.create_dir ~prefix ~suffix >>= fun dn ->
  protect ~finally:(rm_rf dn) (reify_exn f dn)

let sexp_of_any _ = Sexp.Atom "_"

let new_channel =
  let prim =
    { Prim.
      name   = "new-channel"
    ; args   = []
    ; result = F sexp_of_any
    ; run    = fun _ -> Event.new_channel ()
    }
  in
  Prim { prim; args = A0 () }

let sync =
  let prim =
    { Prim.
      name   = "sync"
    ; args   = [A sexp_of_any]
    ; result = F sexp_of_any
    ; run    = fun _ ev -> Event.sync ev
    }
  in
  fun ev -> pack1 prim ev

let sleep =
  let prim =
    Prim.make "sleep" [A sexp_of_float] Unit (fun _ d -> Unix.sleepf d)
  in
  fun d -> pack1 prim d

module Infix = struct
  include Infix0

  let ( |- ) a b = pipe      a b
  let ( |+ ) a b = pipe_both a b
end

module List = List0

module Let_syntax = struct
  let return = return
  include Infix

  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    let both = fork
    module Open_on_rhs = struct end
  end
end
