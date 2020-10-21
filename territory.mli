(** The representation of a territory *)
type t

type territory_name = string

exception UnknownTerritory of territory_name

type territory_owner = string

type troop_count = int

type territory_neighbors = territory_name list

val init : Yojson.Basic.t -> t

val name : t -> territory_name

(*

(** Raised when there's an unknown territory encountered. *)
exception UnknownTerritory of territory_name

(** The region of the territory *)
type territory_region = string

(** The owner of the territory *)
type territory_owner = string

(** The number of troops in the territory. It is always a natural number once the game begins. *)
type troop_count = int

(** The neighbors of the territory *)
type territory_neighbors = territory_name list

(** [name terr] is the name of territory [terr] *)
val name : t -> territory_name

(** [region terr] is the name of the region of territory [terr] *)
val region : t -> territory_region

(** [neighbors terr] is the list of neighbors' names of territory*)
val neighbors: t -> territory_neighbors

(** [owner terr] is the owner of territory [terr] *)
val owner : t -> territory_owner

(** [count terr] is the number of troops in territory [terr] *)
val troop_count : t -> troop_count

*)

