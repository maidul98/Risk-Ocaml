type player_list = Player.t list

type territory_assoc = (Territory.territory_name * Player.t) list

val print_map: territory_assoc -> unit
