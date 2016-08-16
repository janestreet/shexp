open Import

module type S = Debugger_intf.S

module Counter = struct
  type t =
    { mutable value : int
    ; mutex         : Mutex.t
    }

  let create initial =
    { value = initial
    ; mutex = Mutex.create ()
    }

  let get_and_add t x =
    Mutex.lock t.mutex;
    let n = t.value in
    t.value <- n + x;
    Mutex.unlock t.mutex;
    n

  let next t = get_and_add t 1
end

let lines_of_raw_backtrace t =
  let rec loop i acc =
    if i < 0 then
      acc
    else
      let slot =
        Printexc.get_raw_backtrace_slot t i
        |> Printexc.convert_raw_backtrace_slot
      in
      let acc =
        match Printexc.Slot.format i slot with
        | None -> acc
        | Some s -> s :: acc
      in
      loop (i - 1) acc
  in
  let len = Printexc.raw_backtrace_length t in
  loop (len - 1) []


let exn_with_backtrace e bt =
  Sexp.(exn e :: List.map (lines_of_raw_backtrace bt) ~f:string)

module Logger = struct
  type t =
    { out            : Sexp.t -> unit
    ; next_thread_id : Counter.t
    ; thread_id      : int
    ; next_prim_id   : Counter.t
    }

  type (_, _) prim_token = int

  let create out =
    { out
    ; next_thread_id = Counter.create 1
    ; thread_id      = 0
    ; next_prim_id   = Counter.create 0
    }

  let before_prim t prim args =
    let id = Counter.next t.next_prim_id in
    t.out Sexp.(
      List [ List [ Atom "thread" ; int t.thread_id ]
           ; List [ Atom "id"     ; int id          ]
           ; Prim.sexp_of_call prim args
           ]
    );
    id

  let after_prim t prim res id =
    let res : Sexp.t list =
      match res with
      | Ok x ->
        [ Atom "->"
        ; match Prim.sexp_of_result prim x with
        | None -> List []
        | Some s -> s
        ]
      | Error (e, bt) ->
        [ Atom "raised"
        ; List (exn_with_backtrace e bt)
        ]
    in
    t.out Sexp.(
      List (List [ Atom "thread" ; int t.thread_id ] ::
            List [ Atom "id"     ; int id          ] ::
            res)
    )

  let user_exn t e bt =
    t.out Sexp.(
      record [ "thread"   , int t.thread_id
             ; "user-exn" , List (exn_with_backtrace e bt)
             ]
    )

  let fork t =
    let id = Counter.get_and_add t.next_thread_id 2 in
    let t1 = { t with thread_id = id     } in
    let t2 = { t with thread_id = id + 1 } in
    t.out Sexp.(
      cstr_record "fork"
        [ "thread1", int t1.thread_id
        ; "thread2", int t2.thread_id
        ]
    );
    (t1, t2)

  let end_fork t t1 t2 =
    t.out Sexp.(
      cstr_record "end-fork"
        [ "thread1", int t1.thread_id
        ; "thread2", int t2.thread_id
        ]
    )

  let output t s =
    t.out (Atom s)

  let enter_sub _ = ()
  let leave_sub _ = ()
  let force_threads = false
end

module Tracer = struct
  type t =
    { mutable stack       : Sexp.t list
    ; mutable length      : int
    ; mutable checkpoints : int list
    }

  type (_, _) prim_token = unit

  let create () =
    { stack       = []
    ; length      = 0
    ; checkpoints = []
    }

  let result t = Sexp.List (List.rev t.stack)

  let add t sexp =
    t.length <- t.length + 1;
    t.stack <- sexp :: t.stack

  let before_prim t prim args = add t (Prim.sexp_of_call prim args)

  let after_prim t prim res () =
    match res with
    | Ok x ->
      (match Prim.sexp_of_result prim x with
       | None -> ()
       | Some sexp -> add t (List [Atom "->"; sexp]))
    | Error (e, bt) ->
      add t Sexp.(cstr "raised" (exn_with_backtrace e bt))

  let user_exn t e bt =
    add t Sexp.(cstr "user-exn" (exn_with_backtrace e bt))

  let output t s =
    add t (Atom s)

  let fork _ = (create (), create ())

  let end_fork t t1 t2 =
    add t (Sexp.cstr "fork" [result t1; result t2])

  let enter_sub t =
    t.checkpoints <- t.length :: t.checkpoints

  let pop_checkpoint t =
    match t.checkpoints with
    | [] -> assert false
    | x :: l -> t.checkpoints <- l; x

  let leave_sub t =
    let rec split stack acc n =
      if n = 0 then
        (acc, stack)
      else
        match stack with
        | [] -> assert false
        | x :: stack -> split stack (x :: acc) (n - 1)
    in
    let checkpoint = pop_checkpoint t in
    let items, stack = split t.stack [] (t.length - checkpoint) in
    t.stack  <- stack;
    t.length <- checkpoint;
    add t (Sexp.cstr "do" items)

  let force_threads = false
end
