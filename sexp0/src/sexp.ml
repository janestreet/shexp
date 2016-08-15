(* This is shared by sexplib and shexp_sexp, so that the s-expression type is the same and
   exception sexpifier registered before sexplib is initialized are taken into account. *)

type t = Atom of string | List of t list

let exn_converters = ref []
let register_exn_converter = ref (fun extension_constructor f ->
  exn_converters := (extension_constructor, f) :: !exn_converters)

let sexp_of_exn = ref (fun exn ->
  let ec = Obj.extension_constructor exn in
  match List.find (fun (ec', _) -> ec = ec') !exn_converters with
  | exception Not_found -> List [Atom (Printexc.to_string exn)]
  | (_, f) -> f exn)
