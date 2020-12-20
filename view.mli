(** This module handles the printing of a Risk map, as well as gathering the
    data to print. *)

(** Abstract representation of list of players *)
type player_list = Player.t list

(** Abstract representation of a dictionary with the key as a name of the
    territory and the value as the player that holds it *)
type territory_assoc = (Territory.territory_name * Player.t) list

(** [assoc_territories p] return the association list where each key is the name
    of a territory and the value is the player who owns the territory *)
val assoc_territories: player_list -> (Territory.territory_name * Player.t) list

(** [print_map t] prints the map into the terminal using the territory
    association list [t] *)
val print_map: territory_assoc -> unit
