open Core
module Process = Shexp_process

module P = Process
include P.Infix

(* Close all fds >= 3. Must be called before creating the context. *)
let () =
  let get_fds () =
    Sys.readdir "/proc/self/fd"
    |> Array.to_list
    |> List.map ~f:int_of_string
    |> List.sort ~compare:Int.compare
  in
  List.iter (get_fds ()) ~f:(fun fd_num ->
    if fd_num >= 3 then
      let fd = Unix.File_descr.of_int fd_num in
      try
        Unix.close fd
      with Unix.Unix_error (EBADF, _, _) ->
        (* We get the fd for the directory itself in this list *)
        ()
  );
  (* Fd 3 is the file descriptor for reading the directory *)
  [%test_result: int list] ~expect:[0; 1; 2; 3] (get_fds ())

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

let find dir =
  let open P in
  printf "\ntmp dir contents:\n"
  >> chdir dir
       (run "find" []
        |- run "sort" []
        |- iter_lines (printf "- %s\n"))
  >> printf "\n"

let rename_current_directory =
  let open P in
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

let rm_rf =
  let open P in
  with_temp_dir ~prefix:"shexp_process_test" ~suffix:tmpdir_suffix
    (fun tmpdir ->
       chdir tmpdir
         (mkdir "test"
          >> rm_rf "test"
          >> find tmpdir))

