open! Core
open! Expect_test_helpers_kernel

open Import

let eval_exn p = Process.eval ~context p

let%expect_test "bind" =
  eval_exn (P.echo "Hello, world!" >>= fun () ->
            P.echo "How are you?");
  [%expect {|
    Hello, world!
    How are you?
  |}]

let%expect_test "fork" =
  let x = eval_exn (P.fork (P.return "a") (P.return "b")) in
  print_s [%sexp (x : string * string)];
  [%expect {| (a b) |}]

let%expect_test "fork blocking" =
  let x =
    eval_exn (P.fork
                (P.return () >>= fun () -> P.return "a")
                (P.return () >>= fun () -> P.return "b"))
  in
  print_s [%sexp (x : string * string)];
  [%expect {| (a b) |}]

let%expect_test "pipe" =
  eval_exn (P.echo "Hello, world!" |- P.run "tr" ["e"; "a"]);
  [%expect {| Hallo, world! |}]

let%expect_test "stdout_to /dev/null" =
  eval_exn (P.stdout_to "/dev/null" (P.echo "Hello, world!"));
  [%expect {| |}]

let%expect_test "capture" =
  let x = eval_exn (P.capture_unit [Stdout] (P.run "echo" ["Hello, world!"])) in
  print_s [%sexp (x : string)];
  [%expect {| "Hello, world!\n" |}]

let%expect_test "unix environment" =
  let x = eval_exn (P.set_env "FOO" "bar" (P.get_env "FOO")) in
  print_s [%sexp (x : string option)];
  [%expect {| (bar) |}]

let%expect_test "rename current directory" =
  eval_exn rename_current_directory;
  [%expect {|
    created dir blah

    tmp dir contents:
    - .
    - ./blah

    created file blah/foo

    tmp dir contents:
    - .
    - ./blah
    - ./blah/foo

    renamed blah to blah-new

    tmp dir contents:
    - .
    - ./blah-new
    - ./blah-new/foo

    physical current working directory after rename: "<tempdir>/blah-new"
    file foo contains "Hello, world!\n"
  |}]

let%expect_test "rm_rf deletes everything" =
  eval_exn rm_rf;
  [%expect {|
      tmp dir contents:
      - .
 |}]

