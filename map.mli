(** The representation of a map *)
type t

(** The name of the map. *)
type map_name = string

(** The list of regions. *)
type regions = string list

(** The list of territories. *)
type territories = Territory.t list

(** Raised when there's an unknown map encountered. *)
exception UnknownMap of map_name

(** [json_to_map json] is the map that represents [json] *)
val json_to_map : Yojson.Basic.t -> t

(** [name m] is the name of map [m] *)
val name : t -> map_name

(** [regions m] is a set-like list of all regions on map [m] *)
val regions : t -> regions

(** [territories m] is a set-like list of all territories on map [m] *)
val territories : t -> territories
