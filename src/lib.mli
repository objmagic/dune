open Import

(** Representation of a library *)
type t

(** {1 Generals} *)

(** For libraries defined in the workspace, this is the [public_name] if
    present or the [name] if not. *)
val name : t -> string

(** All the names the library might be referred by *)
val names : t -> string list

(** Unique identifier for this library. Note that it is only unique in
    the current process. *)
val unique_id : t -> int

(* CR-someday diml: this should be [Path.t list], since some libraries
   have multiple source directories because of [copy_files]. *)
(** Directory where the source files for the library are located. *)
val src_dir : t -> Path.t option

(** Directory where the object files for the library are located. *)
val obj_dir : t -> Path.t

(** Same as [Path.is_local (obj_dir t)] *)
val is_local : t -> bool

(** For local libraries, whether the library should be installed or
    not. For instance optional libraries that have unavailable
    dependencies shouldn't be installed. *)
val should_install : t -> bool

module Resolved_select = struct
  module No_solution_found : sig
    type t =
      { select_form_loc : Loc.t }
  end

  type t =
    { src_fn : (string, No_solution_found.t) result
    ; dst_fn : string
    }
end

(** Resolved dependencies *)
val resolved_selects
  :  t
  -> required_by:With_required_by.Entry.t list Dep.t list
  -> Resolved_select.t list

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

(** {1 Library name resolution} *)

(** Information about a library *)
module Info : sig
  module Deps : sig
    type t =
      | Simple  of string list
      | Complex of Jbuild.Lib_dep.t list
  end

  module Install_status : sig
    type t =
      | Already_installed (** For external libraries *)
      | Ignore
      | Should_install
      | Install_if_dependencies_are_available
  end

  (** Raw description of a library, where dependencies are not
      resolved. *)
  type t =
    { loc              : Loc.t
    ; name             : string
    ; other_names      : string list
    ; src_dir          : Path.t option
    ; obj_dir          : Path.t
    ; version          : string option
    ; synopsis         : string option
    ; archives         : Path.t list Mode.Dict.t
    ; plugins          : Path.t list Mode.Dict.t
    ; stubs            : string option
    ; jsoo_runtime     : string list
    ; requires         : Deps.t
    ; ppx_runtime_deps : string list
    ; install_status   : Install_status.t
    }

  (** Construct a [t] from a library stanza. *)
  val of_library_stanza : dir:Path.t -> Jbuild.Library.t -> t
end

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
    | Dependency_cycle             of t list
    | Conflict                     of Conflict.t
end

exception Error of Error.t With_required_by.t

(** Collection of libraries organized by names *)
module DB : sig
  type lib = t

  (** A database allow to resolve library names *)
  type t

  (** Create a new library database. [resolve] is used to resolve
      library names in this database. If a library is not found, it
      must return [Error reason] where [reason] explains why the
      library is missing, for instance "not found".

      When a library is not found, it is looked up in the parent
      database if any.

      [all] returns the list of names of libraries available in this database.
  *)
  val create
    :  ?parent:t
    -> resolve:(string -> (Info.t, string) result)
    -> all:(unit -> string list)
    -> unit
    -> t

  (** Create a database from a list of library stanzas *)
  val create_from_library_stanzas
    :  ?parent:t
    -> (Path.t * Jbuild.Library.t) list
    -> t

  val find
    :  t
    -> string
    -> (lib, Error.Library_not_available.t) result
  val find_exn
    :  t
    -> string
    -> required_by:With_required_by.Entry.t list
    -> lib

  val mem : t -> string -> bool

  (** Return the list of all libraries in this database. If
      [recursive] is true, also include libraries in parent databases
      recursively. *)
  val all : ?recursive:bool -> t -> lib list
end with type lib := t

(** {1 Dependencies specified by the user} *)

(** These are what is written in the jbuild or META file, not the
    transitive closure *)

val requires
  :  t
  -> required_by:With_required_by.Entry.t list
  -> t list

val ppx_runtime_deps
  :  t
  -> required_by:With_required_by.Entry.t list
  -> t list

(** {1 Transitive closure} *)

module Closure_cache : sig
  type t
  val create : unit -> t
end

(** Take the transitive closure of a list of libraries:

    - the resulting list contains all the given libraries and their
    dependencies recursively

    - the resulting list is sorted in such a way that if a library [a]
    depends on a library [b], then [b] will come first *)
val closure
  :  t list
  -> cache:Closure_cache.t
  -> required_by:With_required_by.Entry.t list
  -> t list

(** Return all the ppx runtime dependencies for the given list of
    libraries and their dependencies, sorted in the same way as
    [closure] *)
val closed_ppx_runtime_deps_of
  :  t list
  -> cache:Closure_cache.t
  -> required_by:With_required_by.Entry.t list
  -> t list

(* val public_name : t -> string option
 * type local = { src: Path.t ; name: string }
 * val local : t -> local option
 * val unique_id : t -> string *)
