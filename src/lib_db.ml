open Import
open Jbuild

module Scope = struct
  type t =
    { scope : Scope.t
    ; db    : Lib.DB.t
    }

  let external_scope t =
    { db = t
    ; scope =
        { libs = String_map.empty
        ; scope = Jbuild.Scope.empty
        }
    }

  let find             t name = Lib.DB.find t.db name
  let lib_is_available t name = Lib.DB.mem t.db name

  let root t = t.scope.root
  let name t =
    Option.value ~default:"" t.scope.scope.name
end

type t =
  { (* This is to implement the scoping described in the manual *)
    by_path        : (Path.t, Scope.t) Hashtbl.t
  ; by_name        : (string, Scope.t) Hashtbl.t
  ; external_scope : Scope.t
  ; anonymous_root : Path.t
  }

let find_scope_by_path table ~anonymous_root ~dir =
  let rec loop d =
    match Hashtbl.find table d with
    | Some scope -> scope
    | None ->
      if Path.is_root d || not (Path.is_local d) then (
        Sexp.code_error "Lib_db.Scope.find_scope_by_dir got an invalid path"
          [ "dir", Path.sexp_of_t dir
          ; "t.anonymous_root", Path.sexp_of_t anonymous_root ]
      );
      let scope = loop (Path.parent d) in
      Hashtbl.add table ~key:d ~data:scope;
      scope
  in
  loop dir

let find_scope_for_jbuild_in t ~dir =
  let scope =
    find_scope_by_path t.by_path ~dir ~anonymous_root:t.anonymous_root
  in
  { With_required_by.
    required_by = [With_required_by.Entry.jbuild_file_in ~dir:jbuild_dir]
  ; data        = scope
  }

let find_scope = Scope.find_scope

module Sopt_map = Map.Make(struct
    type t = string option
    let compare : t -> t -> int = compare
  end)

let create findlib ~scopes ~root internal_libraries =
  let tmp_table =
    (* Initializes the scopes, including [Path.root] so that when
       there are no <pkg>.opam files in parent directories, the scope
       is the whole workspace. *)
    List.iter scopes ~f:(fun (scope : Jbuild.Scope.t) ->
      let lib_scope = { libs = String_map.empty; scope } in
      Option.iter scope.name ~f:(fun name ->
        assert (name <> "");
        assert (not (Hashtbl.mem t.by_name name));
        Hashtbl.add t.by_name ~key:name ~data:lib_scope;
      );
      Hashtbl.add t.by_path ~key:scope.root ~data:lib_scope
    );
  in
  let libs_by_scope =
    List.fold_left internal_libraries ~init:String_map.empty ~f:(fun acc (dir, lib) ->
      match lib.Library.public with
      | None -> acc
      | Some { name; _ } -> String_map.add acc ~key:name ~data:dir)
  in
  let t =
    { findlib
    ; by_public_name   = Hashtbl.create 1024
    ; by_path = Hashtbl.create 1024
    ; installable_internal_libs = String_map.empty
    ; local_public_libs
    ; anonymous_root = root
    ; by_name = Hashtbl.create 1024
    }
  in
  (* Initializes the scopes, including [Path.root] so that when there are no <pkg>.opam
     files in parent directories, the scope is the whole workspace. *)
  List.iter scopes ~f:(fun (scope : Jbuild.Scope.t) ->
    let lib_scope = { libs = String_map.empty; scope } in
    Option.iter scope.name ~f:(fun name ->
      assert (name <> "");
      assert (not (Hashtbl.mem t.by_name name));
      Hashtbl.add t.by_name ~key:name ~data:lib_scope;
    );
    Hashtbl.add t.by_path ~key:scope.root ~data:lib_scope
  );
  let anon_scope = internal_name_scope t ~dir:t.anonymous_root in
  Hashtbl.add t.by_name ~key:"" ~data:anon_scope;
  List.iter internal_libraries ~f:(fun ((dir, lib) as internal) ->
    let scope = internal_name_scope t ~dir in
    scope.libs <- String_map.add scope.libs ~key:lib.Library.name ~data:internal;
    Option.iter lib.public ~f:(fun { name; _ } ->
      match Hashtbl.find t.by_public_name name with
      | None ->
        Hashtbl.add t.by_public_name ~key:name ~data:(Lib.internal internal)
      | Some lib ->
        (* We only populated this table with internal libraries, who always have
           source dir *)
        let dup_path = Option.value_exn (Lib.src_dir lib) in
        let internal_path d = Path.relative d "jbuild" in
        die "Libraries with identical public names %s defined in %a and %a."
          name Path.pp (internal_path dir) Path.pp (internal_path dup_path)
    ));
  compute_instalable_internal_libs t ~internal_libraries

let external_scope = Scope.external_scope

let anonymous_scope t =
  { Scope.
    lib_db = t
  ; scope = internal_name_scope t ~dir:t.anonymous_root
  }

let find_scope_by_name_exn t ~name =
  match Hashtbl.find t.by_name name with
  | None -> die "Invalid scope '%s'" name
  | Some scope -> { Scope.scope ; lib_db = t }
