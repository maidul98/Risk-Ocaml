(* * The representation of a map which includes its regions and name *)
type t

type territories = Territory.t list

type regions = Region.t list

(** [json_to_map json] is the map that [j] represents *)
val json_to_map : Yojson.Basic.t -> t








