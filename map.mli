(* * The representation of a map which includes its regions and name *)
type t


(** An abstract type representing a single region *)
type region

type territory

(** The list of [region]. *)
type regions = region list

type territories_assoc = (string * territory) list

(** [json_to_map json] is the map that [j] represents *)
val json_to_map : Yojson.Basic.t -> t

(** Given a map will return list of regions*)
(* val get_regions : t -> regions *)

(** Will return an assoc list where the key is the name of the territory and 
    the value is the details of that territory *)
val all_territories_assoc: t -> territories_assoc

val get_color: territory -> ANSITerminal.style list

