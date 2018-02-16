open Import

type t =
  { info : Jbuild.Scope_info.t
  ; db   : Lib.DB.t
  }

let root = t.info.root
let name = t.info.name

module DB = struct
  type scope = t

  module Scope_name_map = Map.Make(struct
      type t = string option
      let compare : t -> t -> int = compare
    end)

  type t =
    { by_dir  : (Path.t, scope) Hashtbl.t
    ; by_name : scope Scope_name_map.t
    ; root    : Path.t
    }

  module Sopt_map = Map.Make(struct
      type t = string option
      let compare : t -> t -> int = compare
    end)

  let find_by_dir t dir =
    let rec loop d =
      match Hashtbl.find t.by_dir d with
      | Some scope -> scope
      | None ->
        if Path.is_root d || not (Path.is_local d) then
          Sexp.code_error "Scope.DB.find_by_dir got an invalid path"
            [ "dir"   , Path.sexp_of_t dir
            ; "t.root", Path.sexp_of_t t.anonymous_root
            ];
        let scope = loop (Path.parent d) in
        Hashtbl.add t.by_dir ~key:d ~data:scope;
        scope
    in
    loop dir

  let find_by_name t name =
    match Scope_name_map.find name t.by_name with
    | Some x -> x
    | None ->
      Sexp.code_error "Scope.DB.find_by_name"
        [ "name", name
        ; "root", t.root
        ]

  let create findlib ~scopes ~root ~public_libs private_libs =
    let scopes_info_by_name =
      List.map scopes ~f:(fun (scope : Jbuild.Scope_info.t) ->
        (scope.name, scope))
      |> Scope_name_map.of_alist
      |> function
      | Ok x -> x
      | Error (name, scope1, scope2) ->
        let to_sexp (scope : Jbuild.Scope_info.t) =
          Sexp.To_sexp.(pair string Path.sexp_of_t)
            scope.name scope.path
        in
        Sexp.code_error "Scope.DB.create got two scopes with the same name"
          [ "scope1", to_sexp scope1
          ; "scope2", to_sexp scope2
          ]
    in
    let libs_by_scope_name =
      List.map private_libs ~f:(fun (dir, (lib : Jbuild.Library.t)) ->
        assert (Option.is_none lib.public);
        (lib.scope.name, (dir, lib)))
      |> Scope_name_map.of_alist_multi
    in
    let by_name =
      Scope_name_map.merge scopes_info_by_name libs_by_scope_name
        ~f:(fun name info libs ->
          let info = Option.value_exn info         in
          let libs = Option.value libs ~default:[] in
          let db =
            Lib.DB.create_from_library_stanzas libs
              ~kind:(Private info)
              ~parent:public_libs
              ~unique_name_suffix:info.name
          in
          { info; db }
    in
    let by_dir = Hashtbl.create 1024 in
    Scope_name_map.iter by_name ~f:(fun name scope ->
      Hashtbl.add by_dir ~key:scope.info.root scope);
    { by_name; by_dir; root }
end
