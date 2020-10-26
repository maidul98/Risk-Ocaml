(** The representation of a card *)
type t

(** The name of the card. *)
type card_name = string

(** The list of territories for which the card is valid. *)
type card_locs = Territory.t list

(****** [init name] returns a type t with given name *******)
val init : card_name -> t

(** [get_name c] is the name of card [c] *)
val get_name : t -> card_name

(** [get_valid_locs c] is a set-like list of all valid territories for which
    card [c] works. *)
val get_valid_locs : t -> card_locs

(** Will add a territory to the list of territories this card owns *)
val add_territory : t -> Territory.t -> t
