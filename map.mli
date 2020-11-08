(** The abstract representation of a map *)
type t

(** A list of territories within the map *)
type territories = Territory.t list

(** A list of regions within the map *)
type regions = Region.t list

(** [json_to_map json] returns the map that [json] represents *)
val json_to_map : Yojson.Basic.t -> t

(** [get_territories map] returns all the territories in this map *)
val get_territories : t -> territories

(** [get_territory map terr] returns the Territory.t version of [terr] in [map] *)
val get_territory : t -> string -> Territory.t
