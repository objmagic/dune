open Import
open Jbuild
open Meta

module Pub_name = struct
  type t =
    | Dot of t * string
    | Id  of string

  let parse s =
    match String.split s ~on:'.' with
    | [] -> assert false
    | x :: l ->
      let rec loop acc l =
        match l with
        | [] -> acc
        | x :: l -> loop (Dot (acc, x)) l
      in
      loop (Id x) l

  let rec root = function
    | Dot (t, _) -> root t
    | Id n       -> n

  let to_list =
    let rec loop acc = function
      | Dot (t, n) -> loop (n :: acc) t
      | Id n       -> n :: acc
    in
    fun t -> loop [] t

  let to_string t = String.concat ~sep:"." (to_list t)
end

type item =
    Lib of Lib_db.Scope.t With_required_by.t * Pub_name.t * Library.t

let string_of_deps l =
  List.map l ~f:Lib.name
  |> List.sort l ~cmp:String.compare
  |> String.concat ~sep:" "

let rule var predicates action value =
  Rule { var; predicates; action; value }
let requires ?(preds=[]) pkgs =
  rule "requires" preds Set (string_of_deps pkgs)
let ppx_runtime_deps ?(preds=[]) pkgs =
  rule "ppx_runtime_deps" preds Set (string_of_deps pkgs)
let description s = rule "description" []      Set s
let directory   s = rule "directory"   []      Set s
let archive preds s = rule "archive"   preds Set s
let plugin preds  s = rule "plugin"    preds Set s
let archives ?(preds=[]) lib =
  let archives = Lib.archives lib in
  let plugins  = Lib.plugins  lib in
  let make ps =
    String.concat ~sep:" " (List.map ps ~f:Path.basename)
  in
  [ archive (preds @ [Pos "byte"  ]) (make archives.byte  )
  ; archive (preds @ [Pos "native"]) (make archives.native)
  ; plugin  (preds @ [Pos "byte"  ]) (make plugins .byte  )
  ; plugin  (preds @ [Pos "native"]) (make plugins .native)
  ]

let gen_lib pub_name lib ~closure_cache ~required_by ~version =
  let desc =
    match Lib.synopsis lib with
    | Some s -> s
    | None ->
      (* CR-someday jdimino: wut? this looks old *)
      match (pub_name : Pub_name.t) with
      | Dot (p, "runtime-lib") ->
        sprintf "Runtime library for %s" (Pub_name.to_string p)
      | Dot (p, "expander") ->
        sprintf "Expander for %s" (Pub_name.to_string p)
      | _ -> ""
  in
  let preds =
    match Lib.kind lib with
    | Normal -> []
    | Ppx_rewriter | Ppx_deriver -> [Pos "ppx_driver"]
  in
  let lib_deps = Lib.requires lib ~required_by in
  let ppx_rt_deps = Lib.ppx_runtime_deps lib ~required_by in
  List.concat
    [ version
    ; [ description desc
      ; requires ~preds lib_deps
      ]
    ; archives ~preds lib
    ; (match ppx_rt_deps with
       | [] -> []
       | _ ->
         [ Comment "This is what jbuilder uses to find out the runtime \
                    dependencies of"
         ; Comment "a preprocessor"
         ; ppx_runtime_deps ppx_rt_deps
         ])
    ; (match Lib.kind lib with
       | Normal -> []
       | Ppx_rewriter | Ppx_deriver ->
         (* Deprecated ppx method support *)
         let no_ppx_driver = Neg "ppx_driver" and no_custom_ppx = Neg "custom_ppx" in
         List.concat
           [ [ Comment "This line makes things transparent for people mixing \
                        preprocessors"
             ; Comment "and normal dependencies"
             ; requires ~preds:[no_ppx_driver]
                 (* For the deprecated method, we need to put the
                    transitive closure of the ppx runtime
                    dependencies.

                    We need to do this because [ocamlfind ocamlc
                    -package ppx_foo] will not look for the transitive
                    dependencies of [foo], and the runtime
                    dependencies might be attached to a dependency of
                    [foo] rather than [foo] itself.

                    Sigh...  *)
                 (Lib.closed_ppx_runtime_deps_of (lib :: lib_deps)
                    ~required_by
                    ~closure_cache)
             ]
           ; match Lib.kind lib with
           | Normal -> assert false
           | Ppx_rewriter ->
             [ rule "ppx" [no_ppx_driver; no_custom_ppx]
                 Set "./ppx.exe --as-ppx" ]
           | Ppx_deriver ->
             [ rule "requires" [no_ppx_driver; no_custom_ppx] Add
                 "ppx_deriving"
             ; rule "ppxopt" [no_ppx_driver; no_custom_ppx] Set
                 ("ppx_deriving,package:" ^ Pub_name.to_string pub_name)
             ]
           ]
      )
    ; (match Lib.jsoo_runtime lib with
       | [] -> []
       | l  ->
         let root = Pub_name.root pub_name in
         [ rule "linkopts" [Pos "javascript"] Set
             (List.map l ~f:(fun fn ->
                sprintf "+%s/%s" root (Filename.basename fn))
              |> String.concat ~sep:" ")
         ; rule "jsoo_runtime" [] Set
             (List.map l ~f:Filename.basename
              |> String.concat ~sep:" ")
         ]
      )
    ]

let gen ~package ~version ~meta_path libs =
  let required_by = [With_required_by.Entry.Path meta_path] in
  let version =
    match version with
    | None -> []
    | Some s -> [rule "version" [] Set s]
  in
  let pkgs =
    List.map libs ~f:(fun lib ->
      let lib_deps = Lib_db.Scope.best_lib_dep_names_exn scope
                       lib.buildable.libraries in
      let lib_deps =
        match Preprocess_map.pps lib.buildable.preprocess with
        | [] -> lib_deps
        | pps ->
          lib_deps @
          String_set.elements
            (Lib_db.Scope.all_ppx_runtime_deps_exn scope (List.map pps ~f:Lib_dep.of_pp))
      in
      let ppx_runtime_deps =
        Lib_db.Scope.best_lib_dep_names_exn scope
          (List.map lib.ppx_runtime_libraries ~f:Lib_dep.direct)
      in
      let ppx_runtime_deps_for_deprecated_method = lazy (
        String_set.union
          (String_set.of_list ppx_runtime_deps)
          (Lib_db.Scope.all_ppx_runtime_deps_exn scope lib.buildable.libraries)
        |> String_set.elements)
      in
      (pub_name,
       gen_lib pub_name lib ~lib_deps ~ppx_runtime_deps ~version
         ~ppx_runtime_deps_for_deprecated_method))
  in
  let pkgs =
    List.map pkgs ~f:(fun (pn, meta) ->
      match Pub_name.to_list pn with
      | [] -> assert false
      | _package :: path -> (path, meta))
  in
  let pkgs = List.sort pkgs ~cmp:(fun (a, _) (b, _) -> compare a b) in
  let rec loop name pkgs =
    let entries, sub_pkgs =
      List.partition_map pkgs ~f:(function
        | ([]    , entries) -> Inl entries
        | (x :: p, entries) -> Inr (x, (p, entries)))
    in
    let entries = List.concat entries in
    let subs =
      String_map.of_alist_multi sub_pkgs
      |> String_map.bindings
      |> List.map ~f:(fun (name, pkgs) ->
        let pkg = loop name pkgs in
        Package { pkg with
                  entries = directory name :: pkg.entries
                })
    in
    { name
    ; entries = entries @ subs
    }
  in
  loop package pkgs
