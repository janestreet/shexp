module List = struct
  include ListLabels

  let concat_map l ~f = map l ~f |> concat
end

include (StdLabels : module type of struct include StdLabels end with module List := List)
include MoreLabels

module SMap = struct
  include Map.Make(String)
  let lookup t key =
    match find t key with
    | x -> Some x
    | exception Not_found -> None
end
module SSet = Set.Make(String)

include Shexp_sexp.Std
include Shexp_bigstring_io.Std

external reraise : exn -> _ = "%reraise"

let protectx ~finally ~f x =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; reraise e

let sprintf = Printf.sprintf

let ( ^/ ) = Filename.concat
