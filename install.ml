#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"shexp"
  [ oasis_lib "sexp0"
  ; oasis_lib "shexp_bigstring"
  ; oasis_lib "shexp_bigstring_io"
  ; oasis_lib "shexp_posixat"
  ; oasis_lib "shexp_process"
  ; oasis_lib "shexp_sexp"
  ; oasis_lib "shexp_spawn"
  ; file "META" ~section:"lib"
  ]
