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

(** [init p] is the initial game state *)
val init : Player.t list -> t

(** [get_curr_player g] is the player of the current game state *)
val get_current_player : t -> Player.t

(** Will return the list of players in the game state *)
val get_players : t -> players

(** Will allow the player to attack another territory*)
val attack : t -> Territory.territory_name -> Territory.territory_name -> t

(** Will allow the player to place troops on map *)
val place : t -> Territory.troop_count -> Territory.territory_name -> (t -> Command.command -> t) ->t

(** Will allow the player to move troops from one territory to another*)
val fortify : t -> Territory.troop_count -> Territory.territory_name -> Territory.territory_name -> (t -> Command.command -> t) -> t

(** Will update the game state based on given command*)
val process_state : t -> Command.command -> t

val get_phase : t -> phase

val get_string_phase : phase -> string
