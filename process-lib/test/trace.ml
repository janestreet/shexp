open Core
open Expect_test_helpers_core
open Import

let trace process =
  let _res, trace =
    without_backtrace (fun () -> Process.Traced.eval ~context ~capture:true process)
  in
  print_s (cleanup_sexp trace)
;;

let%expect_test _ =
  trace (P.echo "Hello, world!");
  [%expect {| ((echo "Hello, world!") "Hello, world!\n") |}]
;;

let%expect_test _ =
  trace
    (P.with_temp_dir ~prefix:"shexp-debugging" ~suffix:tmpdir_suffix (fun tmpdir ->
       P.chdir
         tmpdir
         (P.stdout_to "blah" (P.echo "Bonjour les amis")
          >> P.run "cat" [ "blah" ]
          >> (P.run "cat" [ "blah" ]
              |- P.run "sed" [ "s/o/a/g" ]
              |- P.run "sed" [ "s/ /\\n/g" ])
          >> P.echo "C'est finit!")));
  [%expect
    {|
    ((generate-temporary-directory
       (prefix shexp-debugging)
       (suffix <temp-dir>))
     (-> <temp-dir>)
     (do
       (chdir <temp-dir>)
       (open-file (perm 0o666) (flags (O_WRONLY O_CREAT O_TRUNC)) blah)
       (-> (fd <0>))
       (do (set-ios (stdout) (fd <0>)) (echo "Bonjour les amis"))
       (close-fd (fd <0>))
       (run cat (blah))
       "Bonjour les amis\n"
       (-> (Exited 0))
       (create-pipe)
       (-> (
         (fd <0>)
         (fd <1>)))
       (fork
         ((do
            (set-ios (stdout) (fd <1>))
            (create-pipe)
            (-> (
              (fd <2>)
              (fd <3>)))
            (fork
              ((do (set-ios (stdout) (fd <3>)) (run cat (blah)) (-> (Exited 0)))
               (close-fd (fd <3>)))
              ((do (set-ios (stdin) (fd <2>)) (run sed (s/o/a/g)) (-> (Exited 0)))
               (close-fd (fd <2>)))))
          (close-fd (fd <1>)))
         ((do
            (set-ios (stdin) (fd <0>))
            (run sed ("s/ /\\n/g"))
            "Banjaur\nles\namis\n"
            (-> (Exited 0)))
          (close-fd (fd <0>))))
       (echo "C'est finit!")
       "C'est finit!\n")
     (readdir <temp-dir>)
     (-> (blah))
     (lstat <temp-dir>/blah)
     (->    <stats>)
     (rm    <temp-dir>/blah)
     (rmdir <temp-dir>))
    |}]
;;

let%expect_test "multiple errors" =
  trace
    (P.unset_env
       "A"
       (P.unset_env "B" (P.fork (P.get_env_exn "A") (P.get_env_exn "B") >>| ignore)));
  [%expect
    {|
    ((unset-env A)
     (unset-env B)
     (fork
       ((get-env A)
        (-> ())
        (user-exn (Failure "environment variable \"A\" not found")))
       ((get-env B)
        (-> ())
        (user-exn (Failure "environment variable \"B\" not found")))))
    |}]
;;

let%expect_test "rename current directory" =
  trace rename_current_directory;
  [%expect
    {|
    ((generate-temporary-directory
       (prefix shexp_process_test)
       (suffix <temp-dir>))
     (-> <temp-dir>)
     (do
       (chdir <temp-dir>)
       (mkdir blah)
       (echo  "created dir blah")
       "created dir blah\n"
       (echo (n true) "\ntmp dir contents:\n")
       "\ntmp dir contents:\n"
       (do
         (chdir <temp-dir>)
         (create-pipe)
         (-> (
           (fd <0>)
           (fd <1>)))
         (fork
           ((do
              (set-ios (stdout) (fd <1>))
              (create-pipe)
              (-> (
                (fd <2>)
                (fd <3>)))
              (fork
                ((do (set-ios (stdout) (fd <3>)) (run find ()) (-> (Exited 0)))
                 (close-fd (fd <3>)))
                ((do (set-ios (stdin) (fd <2>)) (run sort ()) (-> (Exited 0)))
                 (close-fd (fd <2>)))))
            (close-fd (fd <1>)))
           ((do
              (set-ios (stdin) (fd <0>))
              (read-line)
              (-> (.))
              (echo (n true) "- .\n")
              "- .\n"
              (read-line)
              (-> (./blah))
              (echo (n true) "- ./blah\n")
              "- ./blah\n"
              (read-line)
              (-> ()))
            (close-fd (fd <0>)))))
       (echo (n true) "\n")
       "\n"
       (open-file (perm 0o666) (flags (O_WRONLY O_CREAT O_TRUNC)) blah/foo)
       (-> (fd <4>))
       (do (set-ios (stdout) (fd <4>)) (echo "Hello, world!"))
       (close-fd (fd <4>))
       (echo "created file blah/foo")
       "created file blah/foo\n"
       (echo (n true) "\ntmp dir contents:\n")
       "\ntmp dir contents:\n"
       (do
         (chdir <temp-dir>)
         (create-pipe)
         (-> (
           (fd <0>)
           (fd <1>)))
         (fork
           ((do
              (set-ios (stdout) (fd <1>))
              (create-pipe)
              (-> (
                (fd <2>)
                (fd <3>)))
              (fork
                ((do (set-ios (stdout) (fd <3>)) (run find ()) (-> (Exited 0)))
                 (close-fd (fd <3>)))
                ((do (set-ios (stdin) (fd <2>)) (run sort ()) (-> (Exited 0)))
                 (close-fd (fd <2>)))))
            (close-fd (fd <1>)))
           ((do
              (set-ios (stdin) (fd <0>))
              (read-line)
              (-> (.))
              (echo (n true) "- .\n")
              "- .\n"
              (read-line)
              (-> (./blah))
              (echo (n true) "- ./blah\n")
              "- ./blah\n"
              (read-line)
              (-> (./blah/foo))
              (echo (n true) "- ./blah/foo\n")
              "- ./blah/foo\n"
              (read-line)
              (-> ()))
            (close-fd (fd <0>)))))
       (echo (n true) "\n")
       "\n"
       (chdir blah)
       (rename ../blah ../blah-new)
       (echo "renamed blah to blah-new")
       "renamed blah to blah-new\n"
       (echo (n true) "\ntmp dir contents:\n")
       "\ntmp dir contents:\n"
       (do
         (chdir <temp-dir>)
         (create-pipe)
         (-> (
           (fd <0>)
           (fd <1>)))
         (fork
           ((do
              (set-ios (stdout) (fd <1>))
              (create-pipe)
              (-> (
                (fd <2>)
                (fd <3>)))
              (fork
                ((do (set-ios (stdout) (fd <3>)) (run find ()) (-> (Exited 0)))
                 (close-fd (fd <3>)))
                ((do (set-ios (stdin) (fd <2>)) (run sort ()) (-> (Exited 0)))
                 (close-fd (fd <2>)))))
            (close-fd (fd <1>)))
           ((do
              (set-ios (stdin) (fd <0>))
              (read-line)
              (-> (.))
              (echo (n true) "- .\n")
              "- .\n"
              (read-line)
              (-> (./blah-new))
              (echo (n true) "- ./blah-new\n")
              "- ./blah-new\n"
              (read-line)
              (-> (./blah-new/foo))
              (echo (n true) "- ./blah-new/foo\n")
              "- ./blah-new/foo\n"
              (read-line)
              (-> ()))
            (close-fd (fd <0>)))))
       (echo (n true) "\n")
       "\n"
       (create-pipe)
       (-> (
         (fd <5>)
         (fd <0>)))
       (fork
         ((do (set-ios (stdout) (fd <0>)) (run pwd (-P)) (-> (Exited 0)))
          (close-fd (fd <0>)))
         ((do (set-ios (stdin) (fd <5>)) (read-all) (-> "<temp-dir>/blah-new\n"))
          (close-fd (fd <5>))))
       (echo
         (n true)
         "physical current working directory after rename: \"<tempdir>/blah-new\"\n")
       "physical current working directory after rename: \"<tempdir>/blah-new\"\n"
       (open-file (flags (O_RDONLY)) foo)
       (-> (fd <5>))
       (do (set-ios (stdin) (fd <5>)) (read-all) (-> "Hello, world!\n"))
       (close-fd (fd <5>))
       (echo (n true) "file foo contains \"Hello, world!\\n\"\n")
       "file foo contains \"Hello, world!\\n\"\n")
     (readdir <temp-dir>)
     (-> (blah-new))
     (lstat   <temp-dir>/blah-new)
     (->      <stats>)
     (readdir <temp-dir>/blah-new)
     (-> (foo))
     (lstat <temp-dir>/blah-new/foo)
     (->    <stats>)
     (rm    <temp-dir>/blah-new/foo)
     (rmdir <temp-dir>/blah-new)
     (rmdir <temp-dir>))
    |}]
;;
