(** The representation of a card *)
type t

(** The name of the card. *)
type card_name = string

(** The list of territories for which the card is valid. *)
type card_locs = Territory.t list

(** Raised when there's an unknown map encountered. *)
exception UnknownCard of card_name

(** [name c] is the name of card [c] *)
val name : t -> card_name

(** [valid_locs c] is a set-like list of all valid territories for which card [c] works. *)
val valid_locs : t -> card_locs
