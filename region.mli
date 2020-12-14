(** The abstract representation of a region *)
type t

(** The name of the region *)
type name = string

(** Given a json representation of a single region, it will return type t *)
val init : Yojson.Basic.t -> t


(** [name p] is the name of player [p] *)
val get_region_name : t -> name

(** [troops p] is the total number of owned troops for player [p] *)
val get_bonus : t -> int

(** [territories p] is a set-like list of all territories that player [p] holds
*)
val get_territories : t -> string list
