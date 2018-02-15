open Import

(* +-----------------------------------------------------------------+
   | Raw library information                                         |
   +-----------------------------------------------------------------+ *)

module Info = struct
  module Deps = struct
    type t =
      | Simple  of string list
      | Complex of Jbuild.Lib_dep.t list
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
    ; ppx_used         : string list
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
    let requires : Deps.t =
      let rec loop (deps : Jbuild.Lib_dep.t list) acc =
        match deps with
        | [] -> Some (List.rev acc)
        | Direct name :: deps -> loop (name :: acc) deps
        | Select _ -> None
      in
      match loop conf.libraries [] with
      | Some l -> Simple l
      | None   -> Complex conf.libraries
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
    ; requires         = Complex conf.libraries
    ; ppx_runtime_deps = conf.ppx_runtime_libraries
    ; when_to_install
    }
end

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Error0 = struct
  module Library_not_available = struct
    module Reason = struct: sig
      type t =
        | Not_found
        | Hidden of string
    end

    type nonrec t =
      { name   : string
      ; reason : Reason.t
      }
  end

  module No_solution_found_for_select = struct
    type t = { loc : Loc.t }
  end
end

exception Error of Error.t With_required_by.t

module Resolved_select = struct
  module No_solution_found = struct
    type t =
      { select_form_loc : Loc.t }
  end

  type t =
    { src_fn : (string, No_solution_found.t) result
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
  ; requires         : t list or_error * Resolved_select.t list
  ; ppx_runtime_deps : t list or_error
  ; install_status   : Info.Install_status.t
  ; db               : t
  }

type db =
  { parent  : db option
  ; resolve : string -> (Info.t, Error.Library_not_available.Reason.t) result
  ; table   : (string, resolve_status) Hashtbl.t
  ; all     : string list Lazy.t
  }

and resolve_status =
  | Resolved     of t
  | Initializing of Init.t
  | Failed       of error

and error =
  | Library_not_available        of Library_not_available.t
  | No_solution_found_for_select of No_solution_found_for_select.t
  | Dependency_cycle             of (Path.t * string) list
  | Conflict                     of conflict

and conflict =
  { lib1 : t * With_required_by.Entry.t list
  ; lib2 : t * With_required_by.Entry.t list
  }

and 'a or_error = ('a, error) result

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

(* +-----------------------------------------------------------------+
   | Generals                                                        |
   +-----------------------------------------------------------------+ *)

let name  t = t.name
let names t = t.name :: t.other_names

let unique_id t = t.unique_id

let kind         t = t.kind
let synopsis     t = t.synopsis
let archives     t = t.archives
let plugins      t = t.plugins
let jsoo_runtime t = t.jsoo_runtime

let src_dir t = t.src_dir
let obj_dir t = t.obj_dir

let is_local t = Path.is_local t.obj_dir

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

let jsoo_archives t =
  List.map t.archives.byte ~f:(Path.extend_basename ~suffix:".js")

(* +-----------------------------------------------------------------+
   | Library name resolution                                         |
   +-----------------------------------------------------------------+ *)

let gen_unique_id =
  let next = ref 0 in
  fun () ->
    let n = !next in
    next := n + 1;
    n

(* Special marker to detect dependencies when resolving optional
   libraries.

   If we see this marker in the resolve cache, we know that we have a
   dependency cycle. *)
let loop_marker = Dependency_cycle []
exception Cycle_detected_while_deciding_optional_library_state

module Dep_stack = struct
  type t =
    { stack : Init.t list
    ; seen  : Int_set.t
    }

  let empty =
    { stack = []
    ; seen  = Int_set.empty
    }

  let dependency_cycle (last_id, last_path, last_name) dep_path =
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
    let loop = build_loop [(last_path, last_name)] dep_path in
    raise (Dependency_cycle loop)

  let push t (x : Init.t) =
    let (id, _, _) = x in
    if Int_set.mem id t.seen then dependency_cycle x t.stack;
    let stack = x :: t.stack in
    { stack = x :: t.stack
    ; seen  = Int_set.add id t.seen
    }
end

let rec make db init (info : Info.t) : t =
  let requires = lazy (
    let deps, resolved_select = resolve_deps db info.requires ~stack in
    let deps =
      match deps with
      | Error _ -> deps
      | Ok deps ->
        let ppx_used = resolve_simple_deps db info.ppx_used in
        let 
    in
    (deps, resolved_select)
  in
  { loc              = info.loc
  ; name             = info.loc
  ; other_names      = info.other_names
  ; unique_id        = gen_unique_id ()
  ; kind             = info.kind
  ; src_dir          = info.src_dir
  ; obj_dir          = info.obj_dir
  ; version          = info.version
  ; synopsis         = info.synopsis
  ; archives         = info.archives
  ; plugins          = info.plugins
  ; jsoo_runtime     = info.jsoo_runtime
  ; requires         = Inl { info = requires
  ; ppx_runtime_deps = resolve_simple_deps db info.ppx_runtime_deps
  ; db               = db
  }

and find db name =
  match Hashtbl.find db.resolve_cache name with
  | Some (Dependency_cycle []) ->
    raise_notrace Cycle_detected_while_deciding_optional_library_state
  | Some x -> x
  | None ->
    match db.resolve name with
    | Ok info ->
      let init =
        { Init.
          unique_id = gen_unique_id ()
        ; path      = Option.value info.src_dir ~default:info.obj_dir
        ; 
      let t = make db info in
      let res =
        if not info.optional then begin
          Ok t
        else begin
          Hashtbl.add t.resolve_cache ~key:name ~data:loop_marker;
          match fst (Lazy.force t.requires) with
          | Ok _ -> Ok t
          | Error _ ->
            let msg =
              "ignored as optional and some dependencies \
               are not available"
            in
            Error { name; reason = Hidden msg }
          | exception Cycle_detected_while_deciding_optional_library_state ->
            let msg =
              (* CR-someday diml: improve this error *)
              "ignored as optional and a cycle was detected"
            in
            Error { name; reason = Hidden msg }
        end
      in
      let names = names t in
      if not (List.mem name ~set:names) then
        Sexp.code_error
          "Lib_db.DB.find: resolver result didn't include requested name"
          [ "requested_name", Sexp.To_sexp.string name
          ; "returned_names", Sexp.To_sexp.(list string) name
          ];
      List.iter names ~f:(fun name ->
        Hashtbl.add t.resolve_cache ~key:name ~data:res);
      res
    | Error reason ->
      let res =
        Error (Library_not_available { name; reason = reason })
      in
      let res =
        match db.parent with
        | None -> res
        | Some db ->
          match find db name with
          | Ok _ as res' -> res'
          | Error reason' ->
            if reason = Not_found then
              Error (Library_not_available na')
            else
              res
      in
      Hashtbl.add t.resolve_cache ~key:name ~data:res;
      res

and mem db name =
  match find db name with
  | Ok    _ -> true
  | Error _ -> false

and resolve_simple_deps db names =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | name :: names ->
      match find t name with
      | Ok x -> loop (x :: acc) names
      | Error reason -> Error (Library_not_available reason)
  in
  loop [] names

and resolve_complex_deps db deps =
  let find_all acc names =
    | [] -> Some (List.rev acc)
    | name :: names ->
      match find db name with
      | Ok t -> find_all (t :: acc) names
      | Error _ -> None
  in
  let res, resolved_select_forms =
    List.fold_left deps ~init:(Ok [], []) ~f:(fun (acc_res, acc_select) dep ->
      let res, acc_select =
        match (dep : Jbuild.Lib_dep.t) with
        | Direct name ->
          let res =
            match find db name with
            | Ok _ as res -> res
            | Error e -> Error (Library_not_available (name, e))
          in
          (res, acc_select)
        | Select { result_fn; choices; loc } ->
          let res, src_fn =
            match
              List.find_map choices ~f:(fun { required; forbidden; file } ->
                if String_set.exists forbidden ~f:(mem db) then
                  None
                else
                  Option.map (find_all db (String_set.elements required))
                    ~f:(fun ts -> (ts, file)))
            with
            | Some (ts, file) ->
              (Ok ts, Ok file)
            | None ->
              (Error (No_solution_found_for_select loc),
               Error { Resolved_select.No_solution_found.
                       select_form_loc = loc
                     })
          in
          (res, { Resolved_select. src_fn; dst_fn = result_fn })
      in
      let res =
        match res, acc_res with
        | Ok l, Ok acc -> Ok (List.rev_append l acc)
        | (Error Not_available _ as res), _
        | _, (Error Not_available _ as res) -> res
        | (Error _ as res), _
        | _, (Error _ as res) -> res
      in
      (res, acc_select))
  in
  let res =
    match res with
    | Ok l -> Ok (List.rev l)
    | Error _ -> res
  in
  (res, acc_Select)

and resolve_deps db deps =
  match deps with
  | Simple  names -> (resolve_simple_deps  db names, [])
  | Complex names -> (resolve_complex_deps db names, [])

let deps_exn deps ~required_by =
  match deps with
  | Ok x -> x
  | Error e ->
    raise (Error { data = e; required_by })

let requires t ~required_by =
  deps_exn (fst (Lazy.force t.requires)) ~required_by

let ppx_runtime_deps t ~required_by =
  deps_exn (Lazy.force t.ppx_runtime_deps) ~required_by

module DB = struct
  type t = db

  module Resolution_failure = Resolution_failure

  let create ?parent ~resolve ~all () =
    { parent
    ; resolve
    ; resolve_cache = Hashtbl.create 1024
    ; closure_cache = Hashtbl.create 1024
    ; all = Lazy.from_fun all
    }

  let create_from_library_stanzas ?parent stanzas =
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
    create () ?parent
      ~resolve:(fun name ->
        match String_map.find name map with
        | None -> Error "not found"
        | Some info -> Ok info)
      ~all:(fun () -> String_map.keys map)

  let mem = mem
  let find = find

  let find_exn t name ~required_by =
    match find t name with
    | Ok x -> x
    | Error na ->
      raise (Error { data = Library_not_available na
                   ; required_by
                   })

  let rec all ?(recursive=false) t =
    let l = List.map (Lazy.force t.all) ~f:(find_exn t ~required_by:[]) in
    match recursive, t.parent with
    | true, Some t -> all ~recursive t @ l
    | _ -> l
end

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

module Closure_cache = struct
  (* A key is the list of package unique identifiers. *)
  type t = (int list, (package list, Error.t) result) Hashtbl.t

  let create () = Hashtbl.create 128
end

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

let closure ts ~cache ~required_by =
  match pkgs with
  | [] -> []
  | _ ->
    let key = List.map ts ~f:(fun p -> p.unique_id) in
    match
      Hashtbl.find_or_add t.closure_cache key ~f:(fun _ ->
        let ts = List.map ts ~f:(fun t -> (t, [])) in
        match Closure.top_closure () ts with
        | Ok ts -> check_conflicts ts
        | Error cycle -> Error { With_required_by.
                                 data        = Dependency_cycle cycle
                               ; required_by = []
                               }
        | exception (Error e) -> Error e)
    with
    | Ok ts -> ts
    | Error e ->
      raise (Error { e with required_by = e.required_by @ required_by })

let closed_ppx_runtime_deps_of ts ~cache ~required_by =
  closure ts ~cache ~required_by
  |> List.concat_map ~f:(ppx_runtime_deps ~required_by)
  |> closure ~cache ~required_by

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
