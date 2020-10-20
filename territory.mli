(** The representation of a territory *)
type t

(** The name of the territory *)
type territory_name = string

(** Raised when there's an unknown territory encountered. *)
exception UnknownTerritory of territory_name

(** The region of the territory *)
type territory_region = string

(** The owner of the territory *)
type territory_owner = string

(** The territory color for printing on map*)
type territory_color = ANSITerminal.style

(** The number of troops in the territory. It is always 1+ once the game begins. *)
type troop_count = int

(** The neighbors of the territory *)
type territory_neighbors = territory_name list

(** Given a map, will return all territories in associated list *)
val map_to_territories : Map.t -> ('a * 'b) list

(** [name terr] is the name of territory [terr] *)
val name : t -> territory_name

(** [region terr] is the name of the region of territory [terr] *)
val region : t -> territory_region

(** [owner terr] is the owner of territory [terr] *)
val owner : t -> territory_owner

(** [count terr] is the number of troops in territory [terr] *)
val troop_count : t -> troop_count

