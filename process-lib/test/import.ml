open Core.Std
include Shexp_process.Std

module P = Process
include P.Infix

let context = Process.Context.create ()

let without_backtrace f =
  let recording = Printexc.backtrace_status () in
  if not recording then
    f ()
  else begin
    Printexc.record_backtrace false;
    protect ~finally:(fun () -> Printexc.record_backtrace true) ~f
  end

let tmpdir_suffix = "-END-OF-TMPDIR"
let cleanup_sexp =
  let pat = String.Search_pattern.create tmpdir_suffix in
  let rec map : Sexp.t -> Sexp.t = function
    | List l ->
      if l <> [] && List.for_all l ~f:(function
        | List [Atom s; _] when String.is_prefix s ~prefix:"st_" -> true
        | _ -> false) then
        Atom "<stats>"
      else
        List (List.map l ~f:map)
    | Atom s ->
      Atom (match String.Search_pattern.index pat ~in_:s with
        | None -> s
        | Some i ->
          let i = i + String.length tmpdir_suffix in
          "<temp-dir>" ^ String.sub s ~pos:i ~len:(String.length s - i))
  in
  map

let rename_current_directory =
  let open P in
  let find dir =
    printf "\ntmp dir contents:\n"
    >> chdir dir
         (run "find" []
          |- run "sort" []
          |- iter_lines (printf "- %s\n"))
    >> printf "\n"
  in
  with_temp_dir ~prefix:"shexp_process_test" ~suffix:tmpdir_suffix
    (fun tmpdir ->
       chdir tmpdir
         (mkdir "blah"
          >> echo "created dir blah"
          >> find tmpdir
          >> stdout_to "blah/foo" (echo "Hello, world!")
          >> echo "created file blah/foo"
          >> find tmpdir
          >> chdir "blah"
               (rename "../blah" "../blah-new"
                >> echo "renamed blah to blah-new"
                >> find tmpdir
                >> capture_unit [Stdout] (run "pwd" ["-P"])
                >>= (fun p ->
                  let p = String.strip p in
                  let p =
                    match String.chop_prefix p ~prefix:tmpdir with
                    | None -> p
                    | Some p -> "<tempdir>" ^ p
                  in
                  printf "physical current working directory after rename: %S\n" p)
                >> stdin_from "foo" read_all
                >>= printf "file foo contains %S\n")))

(* Make sure that we always get the same fd numbers in tests: make sure we have a block of
   fds from 0 to 31. *)
let () =
  let block = Int.Set.of_list (List.range 0 32) in
  let rec loop () =
    let fds =
      Sys.readdir "/proc/self/fd"
      |> Array.map ~f:int_of_string
      |> Int.Set.of_array
    in
    [%test_result: Int.Set.t] ~expect:Int.Set.empty
      (Set.diff fds block);
    let to_open = Set.length (Set.diff block fds) in
    if to_open > 0 then begin
      for _ = 1 to to_open do
        ignore (Unix.openfile "/dev/null" ~mode:[O_RDONLY] : Unix.File_descr.t)
      done;
      loop ()
    end
  in
  loop ()
