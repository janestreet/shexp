(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let dispatch = function
  | After_rules ->
    let hack = "ugly_hack_to_workaround_ocamlbuild_nightmare" in
    mark_tag_used hack;
    dep [hack] [hack];

    let add_exts fn exts =
      List.map (fun ext -> fn ^ ext)  exts
    in

    let sexp0_mod = "sexp0/src/sexp" in
    rule hack
      ~prod:hack
      ~deps:(add_exts sexp0_mod [".cmx"; ".cmi"; ".cmo"])
      (fun _ _ ->
         let to_remove =
           add_exts sexp0_mod [ ".cmx"
                              ; ".cmi"
                              ; ".cmo"
                              ; ".ml"
                              ; ".mli"
                              ; ".ml.depends"
                              ; ".mli.depends"
                              ; ".o"
                              ]
         in
         Seq
           [ Seq (List.map rm_f to_remove)
           ; Echo ([], hack) ])

  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    dispatch hook;
    dispatch_default hook)
