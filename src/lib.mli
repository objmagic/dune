open Import

(** {1 Generals} *)

(** Representation of a library *)
type t

(** For libraries defined in the workspace, this is the [public_name] if
    present or the [name] if not. *)
val name : t -> string

(** All the names the library might be referred by *)
val names : t -> string list

(* CR-someday diml: this should be [Path.t list], since some libraries
   have multiple source directories because of [copy_files]. *)
(** Directory where the source files for the library are located. *)
val src_dir : t -> Path.t

(** Directory where the object files for the library are located. *)
val obj_dir : t -> Path.t

(** Same as [Path.is_local (obj_dir t)] *)
val is_local : t -> bool

val synopsis     : t -> string option
val kind         : t -> Jbuild.Library.Kind.t
val archives     : t -> Path.t list Mode.Dict.t
val plugins      : t -> Path.t list Mode.Dict.t
val jsoo_runtime : t -> string list

(** Operations on list of libraries *)
module List : sig
  type nonrec t = t list

  val include_paths : t -> stdlib_dir:Path.t -> Path.Set.t
  val include_flags : t -> stdlib_dir:Path.t -> _ Arg_spec.t

  val c_include_flags : t -> stdlib_dir:Path.t -> _ Arg_spec.t

  val link_flags : t -> mode:Mode.t -> stdlib_dir:Path.t -> _ Arg_spec.t

  (** All the library archive files (.a, .cmxa, _stubs.a, ...)  that
      should be linked in when linking an executable. *)
  val archive_files : t -> mode:Mode.t -> ext_lib:string -> Path.t list

  val jsoo_runtime_files : t -> Path.t list
end

val jsoo_archives : t -> Path.t list

(** {1 Errors} *)

module Error : sig
  module Library_not_available : sig
    module Reason : sig
      type t =
        | Not_found
        | Hidden of string
    end

    type nonrec t =
      { name   : string
      ; reason : Reason.t
      }
  end

  module No_solution_found_for_select : sig
    type t = { loc : Loc.t }
  end

  module Conflict : sig
    (** When two libraries in a transitive closure conflict *)
    type nonrec t =
      { lib1 : t * With_required_by.Entry.t list
      ; lib2 : t * With_required_by.Entry.t list
      }
  end

  type t =
    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of (Path.t * string) list
    | Conflict                     of Conflict.t
end

exception Error of Error.t With_required_by.t

(** {1 Library compilation} *)

(** For compiling the library itself *)
module Compile : sig
  (** Return the list of dependencies needed for compiling this library *)
  val requires
    :  t
    -> (List.t, Error.t With_required_by.t) result

  module Resolved_select : sig
    type t =
      { src_fn : (string, Error.No_solution_found_for_select.t) result
      ; dst_fn : string
      }
  end

  (** Resolved select forms *)
  val resolved_selects
    :  t
    -> required_by:With_required_by.Entry.t list Dep.t list
    -> Resolved_select.t list
end

(** {1 Library name resolution} *)

(** Information about a library *)
module Info : sig
  module Deps : sig
    type t =
      | Simple  of string list
      | Complex of Jbuild.Lib_dep.t list
  end

  (** Raw description of a library, where dependencies are not
      resolved. *)
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
    ; pps              : string list
    ; optional         : bool
    }

  (** Construct a [t] from a library stanza. *)
  val of_library_stanza : dir:Path.t -> Jbuild.Library.t -> t
end

(** Collection of libraries organized by names *)
module DB : sig
  type lib = t

  (** A database allow to resolve library names *)
  type t

  module Kind : sig
    type t =
      | Installed
      (** Installed libraries               *)
      | Public
      (** Public libraries of the workspace *)
      | Private of Jbuild.Scope_info.t
      (** Private libraries of the given scope *)
  end

  val kind : t -> Kind.t

  (** Create a new library database. [resolve] is used to resolve
      library names in this database.

      When a library is not found, it is looked up in the parent
      database if any.

      [all] returns the list of names of libraries available in this database.
  *)
  val create
    :  kind:Kind.t
    -> ?parent:t
    -> resolve:(string -> (Info.t, Error.Library_not_available.Reason.t) result)
    -> all:(unit -> string list)
    -> unit
    -> t

  (** Create a database from a list of library stanzas *)
  val create_from_library_stanzas
    :  kind:Kind.t
    -> ?parent:t
    -> (Path.t * Jbuild.Library.t) list
    -> t

  val find : t -> string -> lib option
  val find_exn
    :  t
    -> string
    -> required_by:With_required_by.Entry.t list
    -> lib

  val mem : t -> string -> bool

  (** Resolve libraries written by the user in a jbuild file. The
      resulting list of libraries is transitively closed and sorted by
      order of dependencies.

      This function is for executables stanzas.  *)
  val resolve_user_written_deps
    :  t
    -> Jbuild.Lib_dep.t list
    -> pps:string list
    -> (List.t, Error.t With_required_by.t) result * Resolved_select.t list

  (** Return the list of all libraries in this database. If
      [recursive] is true, also include libraries in parent databases
      recursively. *)
  val all : ?recursive:bool -> t -> lib list
end with type lib := t

(** The database this library is part of *)
val db_name : t -> DB.t

(** {1 Dependencies for META files} *)

module Meta : sig
  val requires
    :  t
    -> required_by:With_required_by.Entry.t list
    -> String_set.t

  val ppx_runtime_deps
    :  t
    -> required_by:With_required_by.Entry.t list
    -> String_set.t

  val ppx_runtime_deps_for_deprecated_method
    :  t
    -> required_by:With_required_by.Entry.t list
    -> String_set.t
end

(* val public_name : t -> string option
 * type local = { src: Path.t ; name: string }
 * val local : t -> local option
 * val unique_id : t -> string *)
