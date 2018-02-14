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

(** Findlib database as library database *)
val db : t -> Lib.DB.t

(** [root_package_name "foo.*"] is "foo" *)
val root_package_name : string -> string

module Package : sig
  (** Representation of a findlib package *)
  type t

  val name        : t -> string
  val dir         : t -> Path.t
  val version     : t -> string option
  val description : t -> string option
end

(** List all the packages available in this Database *)
val all_packages  : t -> Package.t list

(** List all the packages that are not available in this database *)
val all_unavailable_packages : t -> Lib.Error.Library_not_available.t list

val stdlib_with_archives : t -> Package.t

module Config : sig
  type t
  val load : Path.t -> toolchain:string -> context:string -> t
  val get : t -> string -> string option
end
