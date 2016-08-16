open! Import

type 'a state =
  | Pending
  | Finished of ('a, exn) result
  | Waiting_for_result of Condition.t

type 'a t =
  { work          : unit -> 'a
  ; mutable state : 'a state
  ; mutex         : Mutex.t
  }

type packed = T : _ t -> packed

let run t =
  let state =
    Finished (
      match t.work () with
      | x -> Ok x
      | exception e -> Error e
    )
  in
  Mutex.lock t.mutex;
  let old_state = t.state in
  t.state <- state;
  (match old_state with
   | Waiting_for_result cond -> Condition.broadcast cond
   | _ -> ());
  Mutex.unlock t.mutex

module Worker = struct
  type t = { next_job : packed Event.channel }

  let workers = Queue.create ()
  let count   = ref 0
  let mutex   = Mutex.create ()

  let rec loop t =
    Mutex.lock mutex;
    Queue.push t workers;
    Mutex.unlock mutex;
    (* Wait for a job *)
    let (T job) = Event.sync (Event.receive t.next_job) in
    run job;
    loop t

  let start job =
    run job;
    loop { next_job = Event.new_channel () }
end

let pid = ref 0

let detach ~f =
  let t =
    { work  = f
    ; state = Pending
    ; mutex = Mutex.create ()
    }
  in
  Mutex.lock Worker.mutex;
  (* Detect forks *)
  let current_pid = Unix.getpid () in
  if !pid <> current_pid then begin
    pid := current_pid;
    Queue.clear Worker.workers;
    Worker.count := 0
  end;
  if not (Queue.is_empty Worker.workers) then begin
    let worker = Queue.pop Worker.workers in
    Mutex.unlock Worker.mutex;
    Event.sync (Event.send worker.next_job (T t));
  end else begin
    let f =
      if !Worker.count = 16 then
        run
      else begin
        incr Worker.count;
        Worker.start
      end
    in
    Mutex.unlock Worker.mutex;
    ignore (Thread.create f t : Thread.t)
  end;
  t

let really_wait t cond =
  Condition.wait cond t.mutex;
  match t.state with
  | Finished res -> res
  | _ -> assert false

let wait t =
  Mutex.lock t.mutex;
  let res =
    match t.state with
    | Finished res -> res
    | Waiting_for_result cond ->
      really_wait t cond
    | Pending ->
      let cond = Condition.create () in
      t.state <- Waiting_for_result cond;
      really_wait t cond
  in
  Mutex.unlock t.mutex;
  match res with
  | Ok x -> x
  | Error e -> raise e
