open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create len = Array1.create char c_layout len

let length (t : t) = Array1.dim t

external unsafe_blit :
  src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  = "shexp_bigstring_blit_stub" [@@noalloc]

external unsafe_blit_string_t :
  src:string -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  = "shexp_bigstring_blit_string_bigstring_stub" [@@noalloc]

external unsafe_blit_t_bytes :
  src:t -> src_pos:int -> dst:Bytes.t -> dst_pos:int -> len:int -> unit
  = "shexp_bigstring_blit_bigstring_bytes_stub" [@@noalloc]

(* See comment in Core_kernel.Ordered_set_lang to convince yourself that it is safe *)
let pos_len_ok ~pos ~len ~length =
  let stop = pos + len in
  pos lor len lor stop lor (length - stop) >= 0

let [@inlined never] out_of_range ~pos ~len ~length =
  Printf.ksprintf failwith
    "Shexp_bigstring got invalid range (pos=%d, len=%d, length=%d)"
    pos len length

let check_pos_len_exn ~pos ~len ~length =
  if not (pos_len_ok ~pos ~len ~length) then
    out_of_range ~pos ~len ~length

let blit ~src ~src_pos ~dst ~dst_pos ~len =
  check_pos_len_exn ~pos:src_pos ~len ~length:(length src);
  check_pos_len_exn ~pos:dst_pos ~len ~length:(length dst);
  unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len

let blit_string_t ~src ~src_pos ~dst ~dst_pos ~len =
  check_pos_len_exn ~pos:src_pos ~len ~length:(String.length src);
  check_pos_len_exn ~pos:dst_pos ~len ~length:(       length dst);
  unsafe_blit_string_t ~src ~src_pos ~dst ~dst_pos ~len

let blit_t_bytes ~src ~src_pos ~dst ~dst_pos ~len =
  check_pos_len_exn ~pos:src_pos ~len ~length:(      length src);
  check_pos_len_exn ~pos:dst_pos ~len ~length:(Bytes.length dst);
  unsafe_blit_t_bytes ~src ~src_pos ~dst ~dst_pos ~len

let sub_string t ~pos ~len =
  check_pos_len_exn ~pos ~len ~length:(length t);
  let res = Bytes.create len in
  unsafe_blit_t_bytes ~src:t ~src_pos:pos ~dst:res ~dst_pos:0 ~len;
  Bytes.unsafe_to_string res

external unsafe_index : t -> pos:int -> len:int -> char:char -> int
  = "shexp_bigstring_index"
external unsafe_rindex : t -> pos:int -> len:int -> char:char -> int
  = "shexp_bigstring_rindex"

let index t ~pos ~len ~char =
  check_pos_len_exn ~pos ~len ~length:(length t);
  match unsafe_index t ~pos ~len ~char with
  | -1 -> None
  | n  -> Some n

let rindex t ~pos ~len ~char =
  check_pos_len_exn ~pos ~len ~length:(length t);
  match unsafe_rindex t ~pos ~len ~char with
  | -1 -> None
  | n  -> Some n

type ('a, 'b) fold_temporary_result =
  | Resize   of { new_size : int; state : 'a }
  | Continue of { state : 'a } (** Same as [Resize] with the same size *)
  | Return of 'b

let template = lazy(create 0)

external create_temporary : template:t -> int -> t =
  "shexp_bigstring_create_temporary"

external destroy_temporary : t -> unit = "shexp_bigstring_destroy_temporary"
external resize_temporary : t -> int -> unit = "shexp_bigstring_resize_temporary"

let fold_temporary ~size ~init ~f =
  let t = create_temporary ~template:(Lazy.force template) size in
  let rec loop acc =
    match f t acc with
    | Resize { new_size; state } ->
      if new_size <> length t then
        resize_temporary t new_size;
      loop state
    | Continue { state } ->
      loop state
    | Return x ->
      destroy_temporary t;
      x
    | exception e ->
      destroy_temporary t;
      raise e
  in
  loop init

let with_temporary ~size ~f =
  fold_temporary ~size ~init:() ~f:(fun t () -> Return (f t))
