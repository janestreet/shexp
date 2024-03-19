open Core
open Expect_test_helpers_core
open Import

let show t =
  without_backtrace (fun () ->
    show_raise ~hide_positions:true (fun () -> Process.eval ~context t))
;;

let%expect_test "command not found" =
  show (P.run "command-that-doesnt-exist" []);
  [%expect {| (raised (Failure "command-that-doesnt-exist: command not found")) |}]
;;

let%expect_test "file not found" =
  show (P.stdin_from "/file-that-doesnt-exist" P.read_all);
  [%expect
    {|
    (raised (
      Unix.Unix_error "No such file or directory" openat /file-that-doesnt-exist))
    |}]
;;

let%expect_test "multiple errors" =
  show
    (P.unset_env "A" (P.unset_env "B" (P.fork (P.get_env_exn "A") (P.get_env_exn "B"))));
  [%expect {| (raised (Failure "environment variable \"A\" not found")) |}]
;;

let%expect_test "command fails" =
  show (P.run "false" []);
  [%expect {| (raised (Failure "Command exited with code 1: false")) |}];
  show (P.run "cat" [ "imaginary-file" ]);
  [%expect
    {|
    cat: imaginary-file: No such file or directory
    (raised (Failure "Command exited with code 1: cat imaginary-file"))
    |}]
;;
