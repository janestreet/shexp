type t = int

(* this function is a copy&paste from stdune *)
let name =
  let table =
    let open Sys in
    [ sigabrt, "ABRT"
    ; sigalrm, "ALRM"
    ; sigfpe, "FPE"
    ; sighup, "HUP"
    ; sigill, "ILL"
    ; sigint, "INT"
    ; sigkill, "KILL"
    ; sigpipe, "PIPE"
    ; sigquit, "QUIT"
    ; sigsegv, "SEGV"
    ; sigterm, "TERM"
    ; sigusr1, "USR1"
    ; sigusr2, "USR2"
    ; sigchld, "CHLD"
    ; sigcont, "CONT"
    ; sigstop, "STOP"
    ; sigtstp, "TSTP"
    ; sigttin, "TTIN"
    ; sigttou, "TTOU"
    ; sigvtalrm, "VTALRM"
    ; sigprof, "PROF"
    ; sigbus, "BUS"
    ; sigpoll, "POLL"
    ; sigsys, "SYS"
    ; sigtrap, "TRAP"
    ; sigurg, "URG"
    ; sigxcpu, "XCPU"
    ; sigxfsz, "XFSZ"
    ]
  in
  fun (n : int) ->
    match List.assoc_opt n table with
    | None -> if n > 0 then Printf.sprintf "%d" n else Printf.sprintf "caml:%d" n
    | Some s -> s
;;
