(** The abstract representation of a territory *)
type t

(** A territory name*)
type territory_name = string

(** The name of the owner of this territory*)
type territory_owner = string

(** The number of troops in this territory*)
type troop_count = int

(** The list of this territory's neighbors' names*)
type territory_neighbors = territory_name list

(** Given a json representation of a single territory, it will return type t*)
val init : Yojson.Basic.t -> t

(** Given a territory, it will return its name*)
val name : t -> territory_name


