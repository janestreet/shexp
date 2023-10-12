open Core
open! Expect_test_helpers_core
open Import

let exec_in_sub f =
  match Unix.fork () with
  | `In_the_child ->
    (try f () with
     | _ -> ());
    Unix.exit_immediately 0
  | `In_the_parent pid -> Unix.waitpid pid |> Unix.Exit_or_signal.or_error |> ok_exn
;;

let%expect_test _ =
  let p = P.fork_unit (P.echo "") (P.echo "") in
  Process.eval ~context p;
  exec_in_sub (fun () -> Process.eval ~context p)
;;

let%expect_test _ =
  Shexp_process.chdir "." (Shexp_process.return ()) |> Shexp_process.eval
;;
