open! Core
open! Expect_test_helpers_kernel

module B = Shexp_bigstring.Std.Bigstring

let%expect_test _ =
  let input =
    String.init 100 ~f:(fun i ->
      let n = i mod 27 in
      if n = 26 then '\n' else Char.of_int_exn (n + Char.to_int 'a'))
  in
  print_string input;
  [%expect{|
    abcdefghijklmnopqrstuvwxyz
    abcdefghijklmnopqrstuvwxyz
    abcdefghijklmnopqrstuvwxyz
    abcdefghijklmnopqrs
  |}];
  let line =
    B.fold_temporary ~size:1 ~init:0 ~f:(fun buf pos ->
      let to_copy = min (B.length buf - pos) (String.length input) in
      B.blit_string_t
        ~src:input ~src_pos:pos
        ~dst:buf   ~dst_pos:pos
        ~len:to_copy;
      match B.index buf ~pos ~len:to_copy ~char:'\n' with
      | None -> Resize { new_size = B.length buf * 2; state = pos + to_copy }
      | Some i -> Return (B.sub_string buf ~pos:0 ~len:i))
  in
  print_string line;
  [%expect {|
    abcdefghijklmnopqrstuvwxyz
  |}]


