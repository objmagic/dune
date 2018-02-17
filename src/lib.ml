open Import

(* +-----------------------------------------------------------------+
   | Raw library information                                         |
   +-----------------------------------------------------------------+ *)

module Info = struct
  module Deps = struct
    type t =
      | Simple  of string list
      | Complex of Jbuild.Lib_dep.t list

    let of_lib_deps deps =
      let rec loop (deps : Jbuild.Lib_dep.t list) acc =
        match deps with
        | [] -> Some (List.rev acc)
        | Direct name :: deps -> loop (name :: acc) deps
        | Select _ -> None
      in
      match loop deps [] with
      | Some l -> Simple l
      | None   -> Complex conf.libraries
  end

  type t =
    { loc              : Loc.t
    ; name             : string
    ; other_names      : string list
    ; kind             : Jbuild.Library.Kind.t
    ; src_dir          : Path.t
    ; obj_dir          : Path.t
    ; version          : string option
    ; synopsis         : string option
    ; archives         : Path.t list Mode.Dict.t
    ; plugins          : Path.t list Mode.Dict.t
    ; stubs            : string option
    ; jsoo_runtime     : string list
    ; requires         : Deps.t
    ; ppx_runtime_deps : string list
    ; pps              : Jbuild.Pp.t list
    ; optional         : bool
    }

  let of_library_stanza ~dir (conf : Jbuild.Library.t) =
    let name, other_names =
      match conf.public with
      | None -> (conf.name, [])
      | Some p -> (p.name, [conf.name])
    in
    let archive_file ext = Path.relative dir (conf.name ^ ext) in
    let archive_files ~f_ext =
      Mode.Dict.of_func (fun mode -> [archive_file (f mode)])
    in
    let stubs =
      if Jbuild.Library.has_stubs conf then
        Some (Jbuild.Library.stubs_archives conf ~dir ~ext_lib:"")
      else
        None
    in
    let jsoo_runtime =
      List.map conf.js_of_ocaml.javascript_files ~f:(Path.relative dir)
    in
    { loc = conf.buildable.loc
    ; name
    ; other_names
    ; kind     = conf.kind
    ; src_dir  = dir
    ; obj_dir  = Utils.library_object_directory ~dir conf.name
    ; version  = None
    ; synopsis = conf.synopsis
    ; archives = archive_files ~f:Mode.compiled_lib_ext
    ; plugins  = archive_files ~f:Mode.plugin_ext
    ; optional = conf.optional
    ; stubs
    ; jsoo_runtime
    ; requires         = Deps.of_lib_deps conf.libraries
    ; ppx_runtime_deps = conf.ppx_runtime_libraries
    ; pps = Jbuild.Preprocess_map.pps conf.buildable.preprocess
    ; when_to_install
    }

  let of_findlib_package pkg =
    let module P = Findlib.Package in
    { loc              = Loc.in_file (Path.to_string (P.meta_file pkg))
    ; name             = P.name pkg
    ; other_names      = []
    ; kind             = Normal
    ; src_dir          = P.dir pkg
    ; obj_dir          = P.dir pkg
    ; version          = P.version pkg
    ; synopsis         = P.description pkg
    ; archives         = P.archives pkg
    ; plugins          = P.plugins pkg
    ; stubs            = None
    ; jsoo_runtime     = P.jsoo_runtime pkg
    ; requires         = Simple (P.requires pkg)
    ; ppx_runtime_deps = P.ppx_runtime_deps pkg
    ; pps              = []
    ; optional         = false
    }
end

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Error0 = struct
  module Library_not_available = struct
    module Reason = Findlib.Unavailable_reason

    type nonrec t =
      { name   : string
      ; reason : Reason.t
      }
  end

  module No_solution_found_for_select = struct
    type t = { loc : Loc.t }
  end
end

module Resolved_select = struct
  type t =
    { src_fn : (string, Error0.No_solution_found_for_select.t) result
    ; dst_fn : string
    }
end

module Init = struct
  type t =
    { unique_id : int
    ; path      : Path.t
    ; name      : string
    }
end

module DB_kind = struct
  type t =
    | Installed
    | Public
    | Private of Jbuild.Scope_info.t
end

type t =
  { loc              : Loc.t
  ; name             : string
  ; other_names      : string list
  ; unique_id        : int
  ; kind             : Jbuild.Library.Kind.t
  ; src_dir          : Path.t
  ; obj_dir          : Path.t
  ; version          : string option
  ; synopsis         : string option
  ; archives         : Path.t list Mode.Dict.t
  ; plugins          : Path.t list Mode.Dict.t
  ; jsoo_runtime     : string list
  ; requires         : t list or_error
  ; ppx_runtime_deps : t list or_error
  ; resolved_select  : Resolved_select.t list
  ; install_status   : Info.Install_status.t
  ; db               : t
  }

type db =
  { parent  : db option
  ; resolve : string -> (Info.t, Error.Library_not_available.Reason.t) result
  ; table   : (string, resolve_status) Hashtbl.t
  ; all     : string list Lazy.t
  ; kind    : DB_kind.t
  }

and resolve_status =
  | Initializing of Init.t
  | Done         of (t, Error0.Library_not_available.Reason.t) result

and error =
  | Library_not_available        of Error0.Library_not_available.Reason.t
  | No_solution_found_for_select of No_solution_found_for_select.t
  | Dependency_cycle             of (Path.t * string) list
  | Conflict                     of conflict

and conflict =
  { lib1 : t * With_required_by.Entry.t list
  ; lib2 : t * With_required_by.Entry.t list
  }

and 'a or_error = ('a, error With_required_by.t) result

module Error = struct
  include Error0

  module Conflict = struct
    type nonrec t = conflict =
      { lib1 : t * With_required_by.Entry.t list
      ; lib2 : t * With_required_by.Entry.t list
      }
  end

  type t = error =
    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of t list
    | Conflict                     of Conflict.t
end

exception Error of Error.t With_required_by.t

(* +-----------------------------------------------------------------+
   | Generals                                                        |
   +-----------------------------------------------------------------+ *)

let name  t = t.name
let names t = t.name :: t.other_names

let kind         t = t.kind
let synopsis     t = t.synopsis
let archives     t = t.archives
let plugins      t = t.plugins
let jsoo_runtime t = t.jsoo_runtime

let src_dir t = t.src_dir
let obj_dir t = t.obj_dir

let is_local t = Path.is_local t.obj_dir

let to_init t : Init.t =
  { unique_id = t.unique_id
  ; path      = t.src_dir
  ; name      = t.name
  }

module List = struct
  type nonrec t = t list

  let include_paths ts ~stdlib_dir =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
      Path.Set.add (obj_dir t) acc)
    |> Path.Set.remove stdlib_dir

  let include_flags ts ~stdlib_dir =
    let dirs = include_paths ts ~stdlib_dir in
    Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
      [Arg_spec.A "-I"; Path dir]))

  let c_include_flags ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add t.src_dir acc)
      |> Path.Set.remove stdlib_dir
    in
    Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
      [Arg_spec.A "-I"; Path dir]))

  let link_flags ts ~mode ~stdlib_dir =
    Arg_spec.S
      (c_include_flags ts ~stdlib_dir ::
       List.map ts ~f:(fun t -> Arg_spec.Deps (Mode.get t.archives mode)))

  let jsoo_runtime_files ts =
    List.concat_map ts ~f:(fun t -> t.jsoo_runtime)

  let stub_archives t ~ext_lib =
    Option.map t.stubs ~f:(fun fn -> Path.extend_basename fn ~suffix:ext_lib)

  let archive_files ts ~mode ~ext_lib =
    List.concat_map ts ~f:(fun t ->
      let l = Mode.Dict.get t.archives mode in
      let l =
        (* Guarded by [is_local t] because we don't know how these
           files are named for external libraries *)
        if mode = Native && is_local t then
          Path.relative dir (t.name ^ ext_lib) :: l
        else
          l
      in
      match stub_archives t ~ext_lib with
      | None -> l
      | Some p -> p :: l)
end

(* +-----------------------------------------------------------------+
   | Library name resolution and transitive closure                  |
   +-----------------------------------------------------------------+ *)

let gen_unique_id =
  let next = ref 0 in
  fun () ->
    let n = !next in
    next := n + 1;
    n

(* Dependency stack used while resolving the dependencies of a library
   that was just returned by the [resolve] callback *)
module Dep_stack = struct
  type t =
    { stack : Init.t list
    ; seen  : Int_set.t
    }

  let empty =
    { stack = []
    ; seen  = Int_set.empty
    }

  let to_requires_by stack =
    List.map stack ~f:(fun { Init.path; name; _ } ->
      With_required_by.Entry.Library (path, name))

  let to_with_required_by t data =
    { With_required_by.
      data
    ; required_by = to_requires_by t.stack
    }

  (* pre-condition: [Int_set.mem x.unique_id t.seem] *)
  let dependency_cycle t (last : Init.t) =
    let rec build_loop acc stack =
      match stack with
      | [] -> assert false
      | (id, path, name) :: stack ->
        let acc = (path, name) :: acc in
        if id = last_id then
          acc
        else
          build_loop acc stack
    in
    let loop = build_loop [(last_path, last_name)] t.stack in
    { data        = Dependency_cycle loop
    ; required_by = []
    }

  (* pre-condition: [not (Int_set.mem x.unique_id t.seem)] *)
  let push_fresh t (x : Init.t) =
    { stack = x :: t.stack
    ; seen  = Int_set.add id t.seen
    }

  let push t (x : Init.t) =
    if Int_mem.set x.unique_id t.seen then
      Error (dependency_cycle t x)
    else
      Ok (push_fresh t x)
end

let ( >>= ) res f =
  match res with
  | Error _ as res -> res
  | Ok x -> f x

let rec make db (info : Info.t) ~unique_id ~stack =
  let requires, resolved_select =
    resolve_user_deps db info.requires ~pps:info.pps ~stack
  in
  let ppx_runtime_deps =
    resolve_simple_deps db info.ppx_runtime_deps ~stack
  in
  let map_error x =
    Result.map_error x ~f:(fun e ->
      With_required_by.prepend_one e (Library (info.src_dir, info.name)))
  in
  let requires         = map_error requires         in
  let ppx_runtime_deps = map_error ppx_runtime_deps in
  { loc              = info.loc
  ; name             = info.loc
  ; other_names      = info.other_names
  ; unique_id        = unique_id
  ; kind             = info.kind
  ; src_dir          = info.src_dir
  ; obj_dir          = info.obj_dir
  ; version          = info.version
  ; synopsis         = info.synopsis
  ; archives         = info.archives
  ; plugins          = info.plugins
  ; jsoo_runtime     = info.jsoo_runtime
  ; requires         = requires
  ; ppx_runtime_deps = ppx_runtime_deps
  ; resolved_select  = resolved_select
  ; db               = db
  }

and find_internal db name ~stack =
  match Hashtbl.find db.resolve_cache name with
  | Some (Initializing init) ->
    Error (Dep_stack.dependency_cycle stack init)
  | Some (Done x) -> x
  | None ->
    match db.resolve name with
    | Ok info ->
      let names = info.name :: info.other_names in
      if not (List.mem name ~set:names) then
        Sexp.code_error
          "Lib_db.DB: resolver result didn't include requested name"
          [ "requested_name", Sexp.To_sexp.string name
          ; "returned_names", Sexp.To_sexp.(list string) name
          ];
      let unique_id = get_unique_id () in
      let init =
        { Init.
          unique_id = gen_unique_id ()
        ; path      = Option.value info.src_dir ~default:info.obj_dir
        ; name      = name
        }
      in
      let stack = Dep_stack.push_fresh stack init in
      (* Add [init] to the table, to detect loops *)
      List.iter names ~f:(fun name ->
        Option.iter (Hashtbl.find t.table name) ~f:(fun t ->
          let to_sexp = Sexp.To_sexp.(pair Path.sexp_of_t (list string)) in
          Sexp.code_error
            "Lib_db.DB: resolver returned name that's already in the table"
            [ "returned_lib"    , to_sexp (info.path, names)
            ; "conflicting_with", to_sexp (t.path, names t)
            ]);
        let init = if name = init.name then init else { init with name } in
        Hashtbl.add t.table ~key:name ~data:init);
      let t = make db info ~unique_id ~stack in
      let res =
        if not info.optional ||
           (Result.is_ok t.requires && Result.is_ok t.ppx_runtime_deps) then
          Ok t
        else
          Error (Hidden "ignored (optional with unavailable dependencies)")
      in
      List.iter names ~f:(fun name ->
        Hashtbl.replace t.resolve_cache ~key:name ~data:res);
      res
    | Error reason as res ->
      let res =
        match db.parent with
        | None -> res
        | Some db ->
          let res' = find_internal db name ~stack in
          match res' with
          | Ok _ -> res'
          | Error reason' ->
            if reason = Not_found then
              res'
            else
              res
      in
      Hashtbl.add t.resolve_cache ~key:name ~data:res;
      res

and mem_internal db name ~stack =
  match find_internal db name ~stack with
  | Ok    _ -> true
  | Error _ -> false

and resolve_simple_deps db names ~stack =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | name :: names ->
      find_internal db name ~stack >>= fun x ->
      loop (x :: acc) names
  in
  loop [] names

and resolve_complex_deps db deps ~stack =
  let res, resolved_select_forms =
    List.fold_left deps ~init:(Ok [], []) ~f:(fun (acc_res, acc_select) dep ->
      let res, acc_select =
        match (dep : Jbuild.Lib_dep.t) with
        | Direct name ->
          (find_internal db name ~stack, acc_select)
        | Select { result_fn; choices; loc } ->
          let res, src_fn =
            match
              List.find_map choices ~f:(fun { required; forbidden; file } ->
                if String_set.exists forbidden ~f:(mem db ~stack) then
                  None
                else
                  match
                    resolve_simple_deps db (String_set.elements required) ~stack
                  with
                  | Ok ts -> Some (ts, file)
                  | Error _ -> None)
            with
            | Some (ts, file) ->
              (Ok ts, Ok file)
            | None ->
              let e = { Error.No_solution_found_for_select.loc } in
              (Error (No_solution_found_for_select e), Error e)
          in
          (res, { Resolved_select. src_fn; dst_fn = result_fn })
      in
      let res =
        match res, acc_res with
        | Ok l, Ok acc -> Ok (List.rev_append l acc)
        | (Error _ as res), _
        | _, (Error _ as res) -> res
      in
      (res, acc_select))
  in
  let res =
    match res with
    | Ok    l -> Ok (List.rev l)
    | Error _ -> res
  in
  (res, acc_select)

and resolve_deps db deps ~stack =
  match deps with
  | Simple  names -> (resolve_simple_deps  db names ~stack, [])
  | Complex names ->  resolve_complex_deps db names ~stack

and resolve_user_deps db deps ~pps ~stack =
  let deps, resolved_select = resolve_deps db deps ~stack ~stack in
  let deps =
    match pps with
    | [] -> deps
    | pps ->
      let pps = List.map pps ~f:Jbuild.Pp.to_string in
      deps >>= fun deps ->
      resolve_simple_deps db pps ~stack >>= fun pps ->
      fold_closure pps ~stack ~init:deps ~f:(fun t acc ->
        ppx_runtime_deps t >>= fun rt_deps ->
        Ok (List.rev_append rt_deps acc))
  in
  (deps, resolved_select)

(* Fold the transitive closure in arbitrary order *)
and fold_closure ts ~init ~f ~stack =
  let seen = ref Int_set.empty in
  let rec loop ts acc ~stack =
    match ts with
    | [] -> acc
    | t :: ts ->
      if Int_set.mem t.unique_id !seen then
        Ok acc
      else begin
        seen := Int_set.add t.unique_id seen;
        f t acc >>= fun acc ->
        (Dep_stack.push stack (to_init t) >>= fun stack ->
         t.requires >>= fun deps ->
         loop deps acc ~stack)
        >>= fun acc ->
        loop ts acc ~stack
      end
  in
  loop ts init ~stack

let to_exn res ~required_by =
  match res with
  | Ok    x -> x
  | Error e -> raise (Error (With_required_by.append e required_by))

let requires_exn t ~required_by =
  to_exn t.requires ~required_by
let ppx_runtime_deps_exn t ~required_by =
  to_exn t.ppx_runtime_deps ~required_by

(* +-----------------------------------------------------------------+
   | Transitive closure                                              |
   +-----------------------------------------------------------------+ *)

module Closure =
  Top_closure.Make
    (String)
    (struct
      type graph = unit
      type nonrec t = t * With_required_by.Entry.t list
      let key (t, _) = t.name
      let deps (t, required_by) () =
        let required_by =
          With_required_by.Entry.Library t.name :: required_by
        in
        List.map (requires pkg ~required_by) ~f:(fun x -> (x, required_by))
    end)

exception Conflict of Error.Conflict.t

let check_conflicts ts ~required_by =
  let add acc name t =
    match String_map.find name acc with
    | None -> String_map.add name ~key:acc ~data:t
    | Some t' -> raise_notrace (Conflict { lib1 = t'; lib2 = t })
  in
  match
    List.fold_left ts ~init:String_map.empty ~f:(fun acc t ->
      List.fold_left (names (fst t)) ~init:acc
        ~f:(fun acc name -> add acc name t))
  with
  | (_ : _ String_map.t) ->
    Ok (List.map ts ~f:fst)
  | exception (Conflict c) ->
    Error { With_required_by.
            data        = Conflict c
          ; required_by = []
          }

let closure_cache = Hashtbl.create 1024

let closure ts =
  match pkgs with
  | [] -> []
  | _ ->
    let key = List.map ts ~f:(fun p -> p.unique_id) in
    Hashtbl.find_or_add closure_cache key ~f:(fun _ ->
      let ts = List.map ts ~f:(fun t -> (t, [])) in
      match Closure.top_closure () ts with
      | Ok ts -> check_conflicts ts
      | Error cycle ->
        Error { With_required_by.
                data        = Dependency_cycle cycle
              ; required_by = []
              }
      | exception (Error e) -> Error e)

let closure_exn ts ~required_by = to_exn (closure ts) ~required_by

module Compile = struct
  let requires t = t.requires >>= closure

  let resolved_select t = t.resolved_select
end

(* +-----------------------------------------------------------------+
   | Databases                                                       |
   +-----------------------------------------------------------------+ *)

module DB = struct
  type t = db

  module Kind = DB_kind
  module Resolution_failure = Resolution_failure

  let kind t = t.kind

  let create ~kind ?parent ~resolve ~all () =
    { kind
    ; parent
    ; resolve
    ; table  = Hashtbl.create 1024
    ; all    = Lazy.from_fun all
    }

  let create_from_library_stanzas ~kind ?parent stanzas =
    let map =
      List.concat_map stanzas ~f:(fun (dir, conf) ->
        let info = Info.of_library_stanza ~dir conf in
        List.map (info.name :: info.other_names) ~f:(fun n -> (n, info)))
      |> String_map.of_alist
      |> function
      | Ok x -> x
      | Error (name, info1, info2) ->
        die "Library %S is defined twice:\n\
             - %s\n\
             - %s"
          name
          (Loc.to_file_colon_line info1.loc)
          (Loc.to_file_colon_line info2.loc)
    in
    create () ?parent ~kind
      ~resolve:(fun name ->
        match String_map.find name map with
        | None -> Error "not found"
        | Some info -> Ok info)
      ~all:(fun () -> String_map.keys map)

  let create_from_findlib findlib =
    create ()
      ~kind:Installed
      ~resolve:(fun name ->
        match Findlib.find findlib name with
        | Ok pkg -> Ok (Info.of_findlib_package pkg)
        | Error _ as res -> res)
      ~all:(fun () ->
        Findlib.all_packages findlib
        |> List.map ~f:Findlib.Package.name)

  let find t name =
    match find_internal t name ~stack:Dep_stack.empty with
    | Ok    x -> Some x
    | Error _ -> None

  let find_exn t name ~required_by =
    to_exn (find_internal t name ~stack:Dep_stack.empty) ~required_by

  let mem t name = mem t name ~stack:Dep_stack.empty

  let resolve_user_written_deps t deps ~pps =
    let res, resolved_select =
      resolve_user_deps t (Info.Deps.of_lib_deps deps) ~pps
        ~stack:Dep_stack.empty
    in
    let res = res >>= closure in
    (res, resolved_select)

  let rec all ?(recursive=false) t =
    let l = List.map (Lazy.force t.all) ~f:(find_exn t ~required_by:[]) in
    match recursive, t.parent with
    | true, Some t -> all ~recursive t @ l
    | _ -> l
end

(* +-----------------------------------------------------------------+
   | META files                                                      |
   +-----------------------------------------------------------------+ *)

module Meta = struct
  let to_names ts =
    List.fold_left ts ~init:String_set.empty ~f:(fun acc t ->
      String_set.add t.name acc)

  (* For the deprecated method, we need to put all the runtime
     dependencies of the transitive closure.

     We need to do this because [ocamlfind ocamlc -package ppx_foo]
     will not look for the transitive dependencies of [foo], and the
     runtime dependencies might be attached to a dependency of [foo]
     rather than [foo] itself.

     Sigh... *)
  let ppx_runtime_deps_for_deprecated_method t ~required_by =
    closure_exn [t] ~required_by
    |> List.concat_map ~f:(ppx_runtime_deps_exn ~required_by)
    |> to_names

  let requires t ~required_by =
    to_names (requires_exn t ~required_by)
  let ppx_runtime_deps t ~required_by =
    to_names (ppx_runtime_deps_exn t ~required_by)
end

(* let describe = function
 *   | Internal (_, lib) ->
 *     sprintf "%s (local)"
 *       (match lib.public with
 *        | Some p -> p.name
 *        | None -> lib.name)
 *   | External pkg ->
 *     sprintf "%s (external)" (FP.name pkg)
 *
 * let ppx_runtime_libraries t ~required_by =
 *   String_set.of_list (
 *     match t with
 *     | Internal (_, lib) -> lib.ppx_runtime_libraries
 *     | External pkg -> List.map ~f:FP.name (FP.ppx_runtime_deps pkg ~required_by)
 *   )
 *
 * let requires t ~required_by =
 *   match t with
 *   | Internal (_, lib) ->
 *     lib.buildable.libraries
 *   | External pkg ->
 *     List.map ~f:(fun fp -> Jbuild.Lib_dep.direct (FP.name fp))
 *       (FP.requires pkg ~required_by)
 *
 * let scope = function
 *   | Internal (dir, _) -> `Dir dir
 *   | External _ -> `External
 *
 * let public_name = function
 *   | External pkg -> Some (FP.name pkg)
 *   | Internal (_, lib) -> Option.map lib.public ~f:(fun p -> p.name)
 *
 * let unique_id = function
 *   | External pkg -> FP.name pkg
 *   | Internal (dir, lib) ->
 *     match lib.public with
 *     | Some p -> p.name
 *     | None -> Path.to_string dir ^ "\000" ^ lib.name
 *
 * type local =
 *   { src: Path.t
 *   ; name: string
 *   }
 *
 * let local = function
 *   | Internal (dir, lib) -> Some { src = dir; name = lib.name }
 *   | External _ -> None
 *
 * let exists_name t ~f =
 *   match t with
 *   | External pkg -> f (FP.name pkg)
 *   | Internal (_, lib) ->
 *     (f lib.name) || (
 *       match lib.public with
 *       | None -> false
 *       | Some p -> f p.name
 *     ) *)
