(** The representation of a player *)
type t

(** The name of the player. *)
type player_name = string

(** The total number of owned troops. *)
type troop_count = int

(** The list of owned territories. *)
type territories = Territory.t list

(** The list of styles to repersent this player *)
type player_style = ANSITerminal.style list

(** Raised when no pair for player can be used to fortify *)
exception No_Fortify

(** Raised when no pair for player can be used to attack *)
exception No_Attack

(** [init name bg_color] is a new player with [name] and [bg_color] *)
val init : player_name -> ANSITerminal.style -> t

(** [get_name p] is the name of [p] *)
val get_name : t -> player_name

(** [get_count p] is the total number of troops owned by [p] *)
val get_count : t -> troop_count

(** [territories p] is a list of all territories owned by [p] *)
val get_territories : t -> territories

(** [get_styles p] are the styles of [p] *)
val get_styles : t -> player_style

(** [get_cards p] is the number of cards owned by [p] *)
val get_cards : t -> int

(** [add_troops c p] is [p] with [c] troops added to [p]'s [troop_count] *)
val add_troops : troop_count -> t -> t

(** [update_troops p] goes through all territories that [p] holds, sums the
    total troop count, and updates the record value *)
val update_troops : t -> unit

(** [add_territory t p] is [p] with [t] added to [p]'s [territories]
    Requires: [t] is not already in [p]'s [territories]
*)
val add_territory : Territory.t -> t -> t

(** [add_territory t p] is [p] with [t] added to [p]'s [territories]
    Requires: [t] is not already in [p]'s [territories]
*)
val add_territory_unit : Territory.t -> t -> unit

(** [del_territory_unit t p] is [p] without [t] added to [p]'s [territories]
    Requires: [t] is not in [p]'s [territories]
*)
val del_territory_unit : Territory.t -> t -> unit

(** [add_card p] increments [p]'s cards by 1 *)
val add_card : t -> unit

(** [set_cards p c] is [p] with [p]'s [cards] set to [c] *)
val set_cards : t -> int -> unit

(** [check_ownership t p] is whether or not [t] is owned by
    [p] *)
val check_ownership : Territory.t -> t -> bool

(** [check_regions p] is a list of regions owned by [p] *)
val check_regions : t -> string list

(** [cash_cards p] is the nummber of cashed-in cards; also updates [p]'s
    [cards] *)
val cash_cards : t -> int

(** [get_random_territory p] is a random territory owned by p *)
val get_random_territory : t -> Territory.t

(** [get_random_territory_and_my_neighbor p] is a pair where the first value is 
    a random territory owned by p and the second value is the name of a 
    territory owned by p *)
val get_random_territory_and_my_neighbor : t -> Territory.t * 
                                                Territory.territory_name

(** [get_random_territory_and_my_neighbor p] is a pair where the first value is 
    a random territory owned by p and the second value is the name of a 
    territory not owned by p *)
val get_random_territory_and_other_neighbor : t -> Territory.t * 
                                                   Territory.territory_name
