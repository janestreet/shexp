open! Core
open! Expect_test_helpers_kernel

open Import

let trace process =
  let _res, trace =
    without_backtrace (fun () ->
      Process.Traced.eval ~context ~capture:true process)
  in
  print_s (cleanup_sexp trace)

let%expect_test _ =
  trace (P.echo "Hello, world!");
  [%expect {|
    ((echo "Hello, world!") "Hello, world!\n")
  |}]

let%expect_test _ =
  trace
    (P.with_temp_dir ~prefix:"shexp-debugging" ~suffix:tmpdir_suffix
       (fun tmpdir ->
          P.chdir tmpdir
            (P.stdout_to "blah" (P.echo "Bonjour les amis")
             >> P.run "cat" ["blah"]
             >> (P.run "cat" ["blah"]
                 |- P.run "sed" ["s/o/a/g"]
                 |- P.run "sed" ["s/ /\\n/g"])
             >> P.echo "C'est finit!")));
  [%expect {|
    ((generate-temporary-directory
       (prefix shexp-debugging)
       (suffix <temp-dir>))
     (-> <temp-dir>)
     (do
       (chdir <temp-dir>)
       (open-file (perm 0o666) (flags (O_WRONLY O_CREAT O_TRUNC)) blah)
       (-> 8)
       (do (set-ios (stdout) 8) (echo "Bonjour les amis"))
       (close-fd 8)
       (run cat (blah))
       "Bonjour les amis\n"
       (-> (Exited 0))
       (create-pipe)
       (-> (8 9))
       (fork
         ((do
            (set-ios (stdout) 9)
            (create-pipe)
            (-> (12 13))
            (fork
              ((do (set-ios (stdout) 13) (run cat (blah)) (-> (Exited 0)))
               (close-fd 13))
              ((do (set-ios (stdin) 12) (run sed (s/o/a/g)) (-> (Exited 0)))
               (close-fd 12))))
          (close-fd 9))
         ((do
            (set-ios (stdin) 8)
            (run sed ("s/ /\\n/g"))
            "Banjaur\nles\namis\n"
            (-> (Exited 0)))
          (close-fd 8)))
       (echo "C'est finit!")
       "C'est finit!\n")
     (chdir   <temp-dir>)
     (readdir .)
     (-> (blah))
     (lstat blah)
     (->    <stats>)
     (rm    blah))
  |}]

let%expect_test "multiple errors" =
  trace
    (P.unset_env "A"
       (P.unset_env "B"
          (P.fork
             (P.get_env_exn "A")
             (P.get_env_exn "B")
           >>| ignore)));
  [%expect {|
    ((unset-env A)
     (unset-env B)
     (fork
       ((get-env A)
        (-> ())
        (user-exn (Failure "environment variable \"A\" not found")))
       ((get-env B)
        (-> ())
        (user-exn (Failure "environment variable \"B\" not found"))))) |}]

let%expect_test "rename current directory" =
  trace rename_current_directory;
  [%expect {|
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
         (-> (9 10))
         (fork
           ((do
              (set-ios (stdout) 10)
              (create-pipe)
              (-> (13 14))
              (fork
                ((do (set-ios (stdout) 14) (run find ()) (-> (Exited 0)))
                 (close-fd 14))
                ((do (set-ios (stdin) 13) (run sort ()) (-> (Exited 0)))
                 (close-fd 13))))
            (close-fd 10))
           ((do
              (set-ios (stdin) 9)
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
            (close-fd 9))))
       (echo (n true) "\n")
       "\n"
       (open-file (perm 0o666) (flags (O_WRONLY O_CREAT O_TRUNC)) blah/foo)
       (-> 8)
       (do (set-ios (stdout) 8) (echo "Hello, world!"))
       (close-fd 8)
       (echo     "created file blah/foo")
       "created file blah/foo\n"
       (echo (n true) "\ntmp dir contents:\n")
       "\ntmp dir contents:\n"
       (do
         (chdir <temp-dir>)
         (create-pipe)
         (-> (9 10))
         (fork
           ((do
              (set-ios (stdout) 10)
              (create-pipe)
              (-> (13 14))
              (fork
                ((do (set-ios (stdout) 14) (run find ()) (-> (Exited 0)))
                 (close-fd 14))
                ((do (set-ios (stdin) 13) (run sort ()) (-> (Exited 0)))
                 (close-fd 13))))
            (close-fd 10))
           ((do
              (set-ios (stdin) 9)
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
            (close-fd 9))))
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
         (-> (9 10))
         (fork
           ((do
              (set-ios (stdout) 10)
              (create-pipe)
              (-> (13 14))
              (fork
                ((do (set-ios (stdout) 14) (run find ()) (-> (Exited 0)))
                 (close-fd 14))
                ((do (set-ios (stdin) 13) (run sort ()) (-> (Exited 0)))
                 (close-fd 13))))
            (close-fd 10))
           ((do
              (set-ios (stdin) 9)
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
            (close-fd 9))))
       (echo (n true) "\n")
       "\n"
       (create-pipe)
       (-> (7 9))
       (fork
         ((do (set-ios (stdout) 9) (run pwd (-P)) (-> (Exited 0))) (close-fd 9))
         ((do (set-ios (stdin) 7) (read-all) (-> "<temp-dir>/blah-new\n"))
          (close-fd 7)))
       (echo
         (n true)
         "physical current working directory after rename: \"<tempdir>/blah-new\"\n")
       "physical current working directory after rename: \"<tempdir>/blah-new\"\n"
       (open-file (flags (O_RDONLY)) foo)
       (-> 7)
       (do (set-ios (stdin) 7) (read-all) (-> "Hello, world!\n"))
       (close-fd 7)
       (echo (n true) "file foo contains \"Hello, world!\\n\"\n")
       "file foo contains \"Hello, world!\\n\"\n")
     (chdir   <temp-dir>)
     (readdir .)
     (-> (blah-new))
     (lstat blah-new)
     (->    <stats>)
     (do
       (chdir   blah-new)
       (readdir .)
       (-> (foo))
       (lstat foo)
       (->    <stats>)
       (rm    foo))
     (rmdir blah-new))
  |}]
