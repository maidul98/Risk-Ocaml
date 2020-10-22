type player_list = Player.t list 

type territory_assoc = (Territory.territory_name * Player.t) list 

(** Is a set like list of territory anme and the player it belongs to.
    Example: [(t1, p1)] where t1 is the terrotiory name and p1 is the [Player] 
    who owns this t1*) 
val assoc_territories : player_list -> territory_assoc

val print_map: territory_assoc -> unit 
