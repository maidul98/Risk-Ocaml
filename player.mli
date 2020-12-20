(** This module represents an abstract player, as well as handles all 
    player data. *)

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
