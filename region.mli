type t 

(** [name reg] returns the name of a region *)
type name = string

val init : Yojson.Basic.t -> t

