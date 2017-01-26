open! Core
open! Expect_test_helpers_kernel

open Import

external sys_exit : int -> _ = "caml_sys_exit"

let exec_in_sub f =
  match Unix.fork () with
  | `In_the_child ->
    (try f () with _ -> ());
    sys_exit 0
  | `In_the_parent pid ->
    Unix.waitpid pid |> Unix.Exit_or_signal.or_error |> ok_exn

let%expect_test _ =
  let p = P.fork_unit (P.echo "") (P.echo "") in
  Process.eval ~context p;
  exec_in_sub (fun () ->
    Process.eval ~context p)
