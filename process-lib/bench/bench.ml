open Core
module Process = Shexp_process
module P = Process
open P.Let_syntax

let context = Process.Context.create ()

module Dbg (M : sig
    val name : string
    val process : unit Process.t
  end) =
struct
  let%bench_module ("" [@name_suffix M.name]) =
    (module struct
      let%bench "no dbg" = Process.eval M.process ~context
      let%bench "traced" = Process.Traced.eval_exn M.process ~context
    end)
  ;;
end

module _ = Dbg (struct
    let name = "many >>="

    let process =
      let rec loop acc n =
        if n = 0
        then acc
        else
          loop
            (let%bind () = P.return () in
             acc)
            (n - 1)
      in
      loop (P.return ()) 1000
    ;;
  end)

module _ = Dbg (struct
    let name = "many set-env"

    let process =
      let rec loop acc n = if n = 0 then acc else loop (P.set_env "X" "" acc) (n - 1) in
      loop (P.return ()) 1000
    ;;
  end)

module _ = Dbg (struct
    let name = "many bind+set-env dyn"

    let process =
      let rec loop n =
        if n = 0
        then P.return ()
        else (
          let%bind () = P.return () in
          P.set_env "X" (string_of_int n) (loop (n - 1)))
      in
      loop 1000
    ;;
  end)

(* Get the stuff in the cache *)
let list_jbuilds =
  P.chdir
    "../../../.."
    (P.stdout_to
       "/dev/null"
       (P.run "find" [ "-print0" ]
        |- P.fold_chunks ~sep:'\000' ~init:[] ~f:(fun l x -> P.return (x :: l))
        >>| List.filter ~f:(fun p -> [%equal: string] (Filename.basename p) "jbuild")
        >>= P.List.iter ~f:P.echo))
;;

let () = Process.eval ~context list_jbuilds

module _ = Dbg (struct
    let name = "list jbuilds"
    let process = list_jbuilds
  end)
