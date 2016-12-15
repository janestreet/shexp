open StdLabels

type t = Base.Exported_for_specific_uses.Sexplib.Sexp.t = Atom of string | List of t list

let list f l = List (List.map l ~f)
let option f o =
  match o with
  | None -> List []
  | Some x -> List [f x]
let int n = Atom (string_of_int n)
let int32 n = Atom (Int32.to_string n)
let int64 n = Atom (Int64.to_string n)
let float n = Atom (string_of_float n)
let string s = Atom s
let char c = Atom (String.make 1 c)
let bool b = Atom (string_of_bool b)

let pair f g (x, y) = List [f x; g y]

let record = list (fun (name, t) -> List [Atom  name; t])

let cstr cstr args = List (Atom cstr :: args)

let cstr_record cstr fields =
  List (Atom cstr :: List.map fields ~f:(fun (name, t) -> List [Atom name; t]))

let cstr_list cstr f l = List (Atom cstr :: List.map l ~f)

let must_escape str =
  let len = String.length str in
  len = 0 ||
  let rec loop ix =
    match str.[ix] with
    | '"' | '(' | ')' | ';' | '\\' -> true
    | '|' -> ix > 0 && let next = ix - 1 in str.[next] = '#' || loop next
    | '#' -> ix > 0 && let next = ix - 1 in str.[next] = '|' || loop next
    | '\000' .. '\032' | '\127' .. '\255' -> true
    | _ -> ix > 0 && loop (ix - 1)
  in
  loop (len - 1)

let sprintf = Printf.sprintf

let rec to_string = function
  | Atom s -> if must_escape s then sprintf "%S" s else s
  | List l -> sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")

let register_exn_converter a b = Base.Exported_for_specific_uses.Sexplib.Conv.Exn_converter.add a b
let exn = Base.Exported_for_specific_uses.Sexplib.Conv.sexp_of_exn
