#!/usr/bin/env ocaml
(* TODO: pass -noinit to ocaml toplevel *)
#use "topfind"
#warnings "+a"
#thread
(* #require "ppx_jane,core" *)
#require "shexp.process"
module List' = List
open Shexp_process
open Shexp_process.Infix
(* open Core *)

(* Check if exe is in PATH *)
let command (exe: string) : bool t =
  find_executable exe >>| fun exe -> exe <> None

(* Finds all files with the given extension in the git repo (or subdirs) *)
let find_files ~(ext: string) : unit t =
  fork (command "git") (command "grep") >>= function
  | true, true ->
    run "git" ["ls-files"]
    |- run "grep" [Printf.sprintf "\\.%s$" ext]
  | git, grep ->
    begin
      if git then echo "Bummer, grep not found in PATH"
      else if grep then echo "Bummer, git not found in PATH"
      else echo "Bummer, both git and grep were not found in PATH"
    end
    >> command "find" >>= function
    | true  ->
      run "find" ["."; "-name"; "*." ^ ext]
    | false ->
      echo "Bummer, find not found in PATH"

(* Counts down from n with a one second delay between ticks *)
let rec countdown (n: int) : unit t =
  if n > 0 then
    echo (string_of_int n)
    >> sleep 1.0
    >> countdown (n-1)
  else
    echo "We'll meet again, Don't know where, don't know when..."

let main : unit t =
  echo "Searching for ml files..."
  >> find_files ~ext:"ml"
  >> echo "------------------------------------------------------------"
  >> echo "Starting countdown..."
  >> countdown 5

let () =
  eval main
