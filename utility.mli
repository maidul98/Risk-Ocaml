(** This module handles complex functions in a Risk game. *)

(** Raised when no pair for player can be used to fortify *)
exception No_Fortify

(** Raised when no pair for player can be used to attack *)
exception No_Attack

(** [territories_from_players p] given a list of players [p] will return all
    territories from all players in [p] into a single list *)
val territories_from_players : Player.t list -> Territory.t list

(** [get_territory_by_name n t] Given a name of a territory [n] and list of
    territories [t], will return territory with the name that matches [n] *)
val get_territory_by_name : Territory.territory_name -> Territory.t list ->
  Territory.t

(** [check_ownership t p] is whether or not [t] is owned by [p] *)
val check_ownership : Territory.t -> Player.t -> bool

(** [is_my_territory p t] returns true if territory with name [t] is owned by
    [p], else false *)
val is_my_territory : Player.t -> Territory.territory_name -> bool

(** [isnt_my_territory p t] returns false if territory with name [t] is owned
    by [p], else true *)
val isnt_my_territory : Player.t -> Territory.territory_name -> bool

(** [get_random_territory p] is a random territory owned by [p] *)
val get_random_territory : Player.t -> Territory.t

(** [get_ai_fortify p] is a pair where the first value is
    a random territory owned by [p] and the second value is the name of a
    territory owned by [p] *)
val get_ai_fortify : Player.t -> Territory.t * Territory.territory_name

(** [get_ai_attack p] is a pair where the first value is
    a random territory owned by [p] and the second value is the name of a
    territory not owned by [p] *)
val get_ai_attack : Player.t -> Territory.t * Territory.territory_name

(** [check_regions p] is a list of regions owned by [p] *)
val check_regions : Player.t -> string list

(** [cash_cards p] is the nummber of cashed-in cards; also updates [p]'s
    [cards] *)
val cash_cards : Player.t -> int
