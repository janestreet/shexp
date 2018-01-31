open! Core
open! Expect_test_helpers_kernel

open Import

let%expect_test _ =
  let log sexp =
    Printf.printf !"%{sexp:Sexp.t}\n%!" (cleanup_sexp sexp)
  in
  Process.Logged.eval ~context ~log
    (P.with_temp_dir ~prefix:"shexp-debugging" ~suffix:tmpdir_suffix
       (fun tmpdir ->
          P.chdir tmpdir
            (P.stdout_to "blah" (P.echo "Bonjour les amis")
             >> P.run "cat" ["blah"]
             >> P.run "sed" ["s/o/a/g"; "blah"]
             >> P.echo "C'est finit!")));
  [%expect {|
    ((thread 0) (id 0)
     (generate-temporary-directory (prefix shexp-debugging) (suffix <temp-dir>)))
    ((thread 0) (id 0) -> <temp-dir>)
    ((thread 0) (id 1) (chdir <temp-dir>))
    ((thread 0) (id 1) -> ())
    ((thread 0) (id 2)
     (open-file (perm 0o666) (flags (O_WRONLY O_CREAT O_TRUNC)) blah))
    ((thread 0) (id 2) -> 8)
    ((thread 0) (id 3) (set-ios (stdout) 8))
    ((thread 0) (id 3) -> ())
    ((thread 0) (id 4) (echo "Bonjour les amis"))
    ((thread 0) (id 4) -> ())
    ((thread 0) (id 5) (close-fd 8))
    ((thread 0) (id 5) -> ())
    ((thread 0) (id 6) (run cat (blah)))
    Bonjour les amis
    ((thread 0) (id 6) -> (Exited 0))
    ((thread 0) (id 7) (run sed (s/o/a/g blah)))
    Banjaur les amis
    ((thread 0) (id 7) -> (Exited 0))
    ((thread 0) (id 8) (echo "C'est finit!"))
    C'est finit!
    ((thread 0) (id 8) -> ())
    ((thread 0) (id 9) (readdir <temp-dir>))
    ((thread 0) (id 9) -> (blah))
    ((thread 0) (id 10) (lstat <temp-dir>/blah))
    ((thread 0) (id 10) -> <stats>)
    ((thread 0) (id 11) (rm <temp-dir>/blah))
    ((thread 0) (id 11) -> ())
    ((thread 0) (id 12) (rmdir <temp-dir>))
    ((thread 0) (id 12) -> ())
  |}]
