(** The abstract representation of a territory *)
type t

(** A territory name *)
type territory_name = string

(** The name of the owner of this territory *)
type territory_owner = string

(** The number of troops in this territory *)
type troop_count = int

(** The list of this territory's neighbors' names *)
type territory_neighbors = territory_name list

(** Given a json representation of a single territory, it will return type t *)
val init : Yojson.Basic.t -> t

(** Given a territory, it will return its name *)
val get_name : t -> territory_name

(** Given a territory, it will return its owner *)
val get_owner : t -> territory_owner

(** Given a territory, it will return its troop count *)
val get_count : t -> troop_count

(** Given a territory, it will return its neighbors *)
val get_neighbors : t -> territory_neighbors

(** Given a territory, it will set the new owner *)
val set_owner : t -> territory_owner -> t

(** Given a territory, it will set the new troop count *)
val set_count : t -> troop_count -> unit
