include Shexp_bigstring.Std.Bigstring

external unsafe_write : Unix.file_descr -> t -> pos:int -> len:int -> int
  = "shexp_bigstring_io_write"
external unsafe_read : Unix.file_descr -> t -> pos:int -> len:int -> int
  = "shexp_bigstring_io_read"

let write fd buf ~pos ~len =
  check_pos_len_exn ~pos ~len ~length:(length buf);
  unsafe_write fd buf ~pos ~len

let read fd buf ~pos ~len =
  check_pos_len_exn ~pos ~len ~length:(length buf);
  unsafe_read fd buf ~pos ~len

let read_all fd =
  fold_temporary ~size:4096 ~init:0 ~f:(fun buf pos ->
    match read fd buf ~pos ~len:(length buf - pos) with
    | 0 -> Return (sub_string buf ~pos:0 ~len:pos)
    | n ->
      let pos = pos + n in
      let size = length buf in
      let new_size =
        if pos = size then (
          let size = size * 2 in
          if size < 0 then max_int else size
        ) else
          size
      in
      Resize { new_size; state = pos }
    | exception Unix.Unix_error (EINTR, _, _) ->
      Continue { state = pos })

let write_all fd s =
  let len = String.length s in
  with_temporary ~size:len ~f:(fun buf ->
    blit_string_t ~src:s ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
    let rec loop pos =
      if pos = len then
        ()
      else
        match write fd buf ~pos ~len:(len - pos) with
        | n -> loop (pos + n)
        | exception Unix.Unix_error (EINTR, _, _) ->
          loop pos
    in
    loop 0)

let read_exactly fd len =
  with_temporary ~size:len ~f:(fun buf ->
    let rec loop pos =
      if pos = len then
        sub_string buf ~pos:0 ~len
      else
        match read fd buf ~pos ~len:(len - pos) with
        | n -> loop (pos + n)
        | exception Unix.Unix_error (EINTR, _, _) ->
          loop pos
    in
    loop 0)

type read_all_interruptible_result =
  { interrupted : bool
  ; collected   : string
  }

let read_all_interruptible ?(delay=1.0) fd ~stop =
  fold_temporary ~size:4096 ~init:0 ~f:(fun buf pos ->
    let size = length buf in
    match Unix.select [fd] [] [] delay with
    | [], [], [] ->
      if stop () then
        Return { interrupted = true
               ; collected   = sub_string buf ~pos:0 ~len:pos
               }
      else
        Continue { state = pos }
    | _ ->
      match read fd buf ~pos ~len:(size - pos) with
      | 0 -> Return { interrupted = false
                    ; collected   = sub_string buf ~pos:0 ~len:pos
                    }
      | n ->
        let pos = pos + n in
        let new_size =
          if pos = size then (
            let size = size * 2 in
            if size < 0 then max_int else size
          ) else
            size
        in
        Resize { new_size; state = pos })

type separator =
  | End_of_line
  | Char of char

let fold_gen fd ~sep ~init ~f =
  fold_temporary ~size:4096 ~init:(0, init) ~f:(fun buf (pos, acc) ->
    match read fd buf ~pos ~len:(length buf - pos) with
    | 0 ->
      if pos = 0 then
        Return acc
      else
        Return (f acc (sub_string buf ~pos:0 ~len:pos))
    | n ->
      let rec loop acc ~start ~pos ~stop : (_, _) fold_temporary_result =
        let char =
          match sep with
          | End_of_line -> '\n'
          | Char c      -> c
        in
        match index buf ~pos ~len:(stop - pos) ~char with
        | None ->
          if start > 0 then
            blit ~src:buf ~dst:buf ~src_pos:start ~dst_pos:0
              ~len:(stop - start);
          let pos = stop - start in
          let size = length buf in
          let new_size =
            if pos = size then (
              let size = size * 2 in
              if size < 0 then max_int else size
            ) else
              size
          in
          Resize { new_size; state = (pos, acc) }
        | Some end_of_chunk ->
          let chunk_len =
            if sep = End_of_line && end_of_chunk > 0 && buf.{end_of_chunk - 1} = '\r' then
              end_of_chunk - 1 - start
            else
              end_of_chunk - start
          in
          let chunk = sub_string buf ~pos:start ~len:chunk_len in
          let acc = f acc chunk in
          loop acc ~start:(end_of_chunk + 1) ~pos:(end_of_chunk + 1) ~stop
      in
      loop acc ~start:0 ~pos ~stop:(pos + n)
    | exception Unix.Unix_error (EINTR, _, _) ->
      Continue { state = (pos, acc) })

let fold_lines fd       ~init ~f = fold_gen fd ~sep:End_of_line ~init ~f
let fold_chunks fd ~sep ~init ~f = fold_gen fd ~sep:(Char sep)  ~init ~f
