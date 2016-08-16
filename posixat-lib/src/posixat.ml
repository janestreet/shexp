include Types

external at_fdcwd : unit -> Fd.t = "shexp_at_fdcwd"

external fstatat
  :  dir:Fd.t
  -> path:string
  -> flags:At_flag.t list
  -> Stats.t
  = "shexp_fstatat"

external readlinkat
  :  dir:Fd.t
  -> path:string
  -> string
  = "shexp_readlinkat"

include Posixat_generated

external fdopendir : Fd.t -> Unix.dir_handle = "shexp_fdopendir"
