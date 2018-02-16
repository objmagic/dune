(** Where libraries are

    This module is used to implement [Super_context.Libs].
*)

open Import

type t

module Scope : sig
  (** A scope can be used to resolve library names to libraries as they are
      defined in the build - external or internal.

      Every directory in the context's build tree corresponds to a particular
      scope which can be found with [find_scope]. The only exception to this is
      the external scope.
  *)

  type t

  val find_lib         : t -> string -> Lib.t option
  val lib_is_available : t -> string -> bool

  val root : t -> Path.t
  val name : t -> string
end

val create
  :  Findlib.t
  -> scopes:Jbuild.Scope.t list
  -> root:Path.t
  -> (Path.t * Jbuild.Library.t) list
  -> t

val find_scope_for_jbuild_in : t -> dir:Path.t -> Scope.t With_required_by.t

(** Includes the private libraries not belonging to any named scope. Corresopnds
    to the context's build root path.*)
val anonymous_scope : t -> Scope.t

(** Contains only publicly, and external (findlib) libraries *)
val external_scope : t -> Scope.t

(** Find scope by the their explicit names (opam package names) [""] corresponds
    to the anonymous scope *)
val find_scope_by_name_exn : t -> name:string -> Scope.t
