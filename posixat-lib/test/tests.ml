open Core.Std
open Shexp_posixat.Std
open! Expect_test_helpers_kernel.Std

module B = Shexp_bigstring_io.Std.Bigstring

let%expect_test _ =
  let tmpdir_prefix = Filename.temp_dir_name ^/ "shexp_posixat_test" in
  let find dir = ignore (ksprintf Sys.command "cd %S && find" dir : int) in
  protectx (Unix.mkdtemp tmpdir_prefix) ~finally:(fun tmpdir ->
    ignore (ksprintf Sys.command "rm -rf %S" tmpdir : int))
    ~f:(fun tmpdir ->
      let tmpdirfd = Unix.openfile tmpdir ~mode:[O_RDONLY] in
      Posixat.mkdirat ~dir:tmpdirfd ~path:"blah" ~perm:0o777;

      find tmpdir;
      [%expect {|
        .
        ./blah
      |}];

      let dirfd = Posixat.openat ~dir:tmpdirfd ~path:"blah" ~flags:[O_RDONLY] ~perm:0 in

      protectx (Posixat.openat ~dir:dirfd ~path:"foo" ~flags:[O_WRONLY; O_CREAT]
                  ~perm:0o777)
        ~finally:Unix.close ~f:(fun fd ->
          let s = "Hello, world!" in
          assert (Unix.write fd ~buf:s = String.length s));

      find tmpdir;
      [%expect {|
        .
        ./blah
        ./blah/foo |}];

      Posixat.renameat ~olddir:tmpdirfd ~oldpath:"blah" ~newdir:tmpdirfd ~newpath:"x";

      find tmpdir;
      [%expect {|
        .
        ./x
        ./x/foo |}];

      protectx (Posixat.openat ~dir:dirfd ~path:"foo" ~flags:[O_RDONLY] ~perm:0o777)
        ~finally:Unix.close ~f:B.read_all
      |> print_string;
      [%expect {|
        Hello, world!
      |}];

      let l =
        protectx (Posixat.fdopendir tmpdirfd) ~finally:Unix.closedir ~f:(fun dh ->
          let rec loop acc =
            match Unix.readdir_opt dh with
            | Some fname -> loop (fname :: acc)
            | None -> List.sort acc ~cmp:String.compare
          in
          loop [])
      in
      print_s [%sexp (l : string list)];
      [%expect {|
        (. .. x)
      |}]
    )
