open Core
open Expect_test_helpers_core
module Unix = Core_unix
module B = Shexp_bigstring_io.Std.Bigstring

let%expect_test _ =
  protectx (Filename_unix.temp_file "test" "") ~finally:Sys_unix.remove ~f:(fun fn ->
    let s =
      String.init 100 ~f:(fun i ->
        let n = i mod 27 in
        if n = 26 then '\n' else Char.of_int_exn (n + Char.to_int 'a'))
    in
    Out_channel.write_all fn ~data:s;
    let with_fd ~f =
      protectx (Unix.openfile fn ~mode:[ O_RDONLY ]) ~finally:Unix.close ~f
    in
    print_string (with_fd ~f:B.read_all);
    [%expect
      {|
      abcdefghijklmnopqrstuvwxyz
      abcdefghijklmnopqrstuvwxyz
      abcdefghijklmnopqrstuvwxyz
      abcdefghijklmnopqrs
      |}];
    with_fd ~f:(B.fold_lines ~init:[] ~f:(fun acc line -> line :: acc))
    |> List.rev
    |> [%sexp_of: string list]
    |> print_s;
    [%expect
      {|
      (abcdefghijklmnopqrstuvwxyz
       abcdefghijklmnopqrstuvwxyz
       abcdefghijklmnopqrstuvwxyz
       abcdefghijklmnopqrs)
      |}])
;;
