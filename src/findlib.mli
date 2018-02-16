(** Findlib database *)

open Import

(** Findlib database *)
type t

val create
  :  stdlib_dir:Path.t
  -> path:Path.t list
  -> t

(** The search path for this DB *)
val path : t -> Path.t list

(** [root_package_name "foo.*"] is "foo" *)
val root_package_name : string -> string

module Package : sig
  (** Representation of a findlib package *)
  type t

  val meta_file        : t -> Path.t
  val name             : t -> string
  val dir              : t -> Path.t
  val version          : t -> string option
  val description      : t -> string option
  val archives         : t -> Path.t list Mode.Dict.t
  val plugins          : t -> Path.t list Mode.Dict.t
  val jsoo_runtime     : t -> string list
  val requires         : t -> string list
  val ppx_runtime_deps : t -> string list
end

module Unavailable_reason : sig
  type t =
    | Not_found
    | Hidden of string
end

(** Lookup a package in the given database *)
val find : t -> string -> (Package.t, Unavailable_reason.t) result

(** List all the packages available in this Database *)
val all_packages  : t -> Package.t list

(** List all the packages that are not available in this database *)
val all_unavailable_packages : t -> (string * Unavailable_reason.t) list

val stdlib_with_archives : t -> Package.t

module Config : sig
  type t
  val load : Path.t -> toolchain:string -> context:string -> t
  val get : t -> string -> string option
end
