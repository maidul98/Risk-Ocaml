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

(****** [init name] returns a type t with given name *******)
val init : player_name -> ANSITerminal.style -> t

(** [name p] is the name of player [p] *)
val get_name : t -> player_name

(** [troops p] is the total number of owned troops for player [p] *)
val get_count : t -> troop_count

(** [territories p] is a set-like list of all territories that player [p] holds
*)
val get_territories : t -> territories

(** Given a player, will return the styles of that player *)
val get_styles : t -> player_style

(** Gets the number of cards that the player owns *)
val get_cards : t -> int

(** Adds [troops_add] troops to player [p] *)
val add_troops : troop_count -> t -> t

(** Will add a territory to the list of territories this player owns.
    Requires: territory is not already in the list *)
val add_territory : Territory.t -> t -> t

(** Adds one card to player, assuming player conquered at least one territory *)
val add_card : t -> unit

(** Sets the number of cards for the player *)
val set_cards : t -> int -> unit

(** [check_ownership terr p] is whether or not [terr] is owned by
    [p] *)
val check_ownership : Territory.t -> t -> bool

(** [check_regions p] is whether or not [p] owns any regions for bonus troops *)
val check_regions : t -> string list

(** [cash_cards p] is cashing in cards for [p] *)
val cash_cards : t -> int
