(** The abstract representation of a region *)
type t

(** The name of the region *)
type name = string

(** [init json] is the region represented by [json] *)
val init : Yojson.Basic.t -> t

(** [get_region_name region] is the name of [region] *)
val get_region_name : t -> name

(** [get_bonus region] is the troop bonus number for [region] *)
val get_bonus : t -> int

(** [get_territories region] are the names of the territories in [region] *)
val get_territories : t -> string list
