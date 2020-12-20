(** This module handles the creation of moves for an Risk-playing AI based on a
    player's data. *)

(** [random_easy_fortify_clause p] creates a fortify clause for the player p *)
val random_easy_fortify_clause : Player.t -> string

(** [random_easy_place_clause p] creates a place clause for the player p *)
val random_easy_place_clause : Player.t -> string

(** [random_easy_attack_clause p] creates a attack clause for the player p *)
val random_easy_attack_clause : Player.t -> string
