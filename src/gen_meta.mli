(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> version:string option
  -> closure_cache:Lib.Closure_cache.t
  -> meta_path:Path.t
  -> Lib.t list
  -> Meta.t
