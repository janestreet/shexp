open! Core
open! Expect_test_helpers_core

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

let%expect_test "fork_all" =
  let x = eval_exn (P.fork_all [P.return "a"; P.return "b"; P.return "c"]) in
  print_s [%sexp (x : string list)];
  [%expect {| (a b c) |}]

let%expect_test "fork_all blocking" =
  let open P.Let_syntax in
  let send ~result ~value ~to_ =
    let%map () = P.sync (Event.send to_ value) in
    result
  in
  let recv ~from = P.sync (Event.receive from) in
  let recv_send ~from ~value ~to_ =
    let%bind result = recv ~from in
    send ~result ~value ~to_
  in
  let process =
    let%bind p1 = P.new_channel in
    let%bind p2 = P.new_channel in
    let%bind p3 = P.new_channel in
    P.fork_all
      [ recv_send ~from:p1  ~value:0 ~to_:p3
      ; send      ~result:1 ~value:2 ~to_:p2
      ; recv_send ~from:p2  ~value:3 ~to_:p1
      ; recv      ~from:p3
      ]
  in
  let x = eval_exn process in
  print_s [%sexp (x : int list)];
  [%expect {| (3 1 2 0) |}]

let%expect_test "fork_unit" =
  let open P.Let_syntax in
  let process =
    let%bind ch = P.new_channel in
    P.fork_all_unit
      [ P.sync (Event.send ch ())
      ; P.sync (Event.receive ch)
      ]
  in
  eval_exn process

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
