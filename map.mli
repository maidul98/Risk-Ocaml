(** This module represents from the data stored in territory and region files.
    It handles loading that data from JSON as well. *)

(** The abstract representation of a map *)
type t

(** A list of territories within the map *)
type territories = Territory.t list

(** A list of regions within the map *)
type regions = Region.t list

(** [json_to_map json] is the map that [json] (equivalent to init in other
    modules) *)
val json_to_map : Yojson.Basic.t -> t

(** [get_territories map] are the territories in [map] *)
val get_territories : t -> territories

(** [get_territory map t_name] is the territory named [t] in [map]
    Requires: [t_name] is a valid name for a territory in [map] *)
val get_territory : t -> string -> Territory.t

(** [get_regions map] are the [regions] of [map] *)
val get_regions : t -> regions
