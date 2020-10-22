(** The abstract representation of a region*)
type t 

(** The name of the region*)
type name = string

(** Given a json representation of a single region, it will return type t  *)
val init : Yojson.Basic.t -> t

