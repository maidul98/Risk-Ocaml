(** The representation for a game state *)
type t

type phase =
  | Attack
  | Fortify
  | Place

(** The list of players participating in the game *)
type players = Player.t list

(** The player whose turn it is at the current game state *)
type current_player = Player.t

(** [init p] is the initial game state containing [p] *)
val init : Player.t list -> t

(** [get_rem_troops g] is the number of remaining troops to be placed enclosed
    in [g] *)
val get_rem_troops : t -> int

(** [get_current_player g] is the current player of [g] *)
val get_current_player : t -> Player.t

(** [get_players g] are the players of [g] *)
val get_players : t -> players

(** [attack g t1 t2] is [g] but with an attack performed from [t1] to [t2] *)
val attack : t -> Territory.territory_name -> Territory.territory_name -> t

(** [place g c t_name f] is [g] but with [c] troops placed on the territory
    named [t_name] *)
val place : t -> Territory.troop_count -> Territory.territory_name -> 
  (t -> Command.command -> t) -> t

(** [fortify g c t1_name t2_name] is [g] but with [c] troops moved from the
    territory named [t1_name] to the territory named [t2_name]  *)
val fortify : t -> Territory.troop_count -> Territory.territory_name -> 
  Territory.territory_name -> (t -> Command.command -> t) -> t

(** [process_state g com] is the new game state based on [g] and [com] *)
val process_state : t -> Command.command -> t

(** [get_phase g] is the phase of [g] *)
val get_phase : t -> phase

(** [get_string_phase phase] is the name that is the string form of [phase] *)
val get_string_phase : phase -> string

(** [troops_round p trade bonus] is the number of troops given to [p]
    considering bonus factors. 
    If [trade] is true, then [troops_round] trades in cards.
    If [p] has 5+ cards, then [troops_round] trades in cards. *)
val troops_round : Player.t -> bool -> int -> int

(* [get_num_terr_owned g] is the number of territories owned by the current 
   player of [g] *)
val get_num_terr_owned : t -> int

(* [check_game_finish g] is whether or not the game
    should be concluded. 
    If all territories of [g] are owned by 1 player, then true
    Otherwise, returns false
*)
val check_game_finish : t -> bool