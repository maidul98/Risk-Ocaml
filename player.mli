(** The representation of a player *)
type t

(** The name of the player. *)
type player_name = string

(** The total number of owned troops. *)
type troops = int

(** The list of owned territories. *)
type territories = Territory.t list

(** The list of owned cards. *)
type cards = Card.t list

(** Raised when there's an unknown player encountered. *)
exception UnknownName of player_name

(** [name p] is the name of player [p] *)
val name : t -> player_name

(** [troops p] is the total number of owned troops for player [p] *)
val count : t -> troops

(** [territories p] is a set-like list of all territories that player [p] holds *)
val territories : t -> territories

(** [cards p] is a set-like list of all cards that player [p] holds *)
val cards : t -> cards
