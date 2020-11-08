(** The representation for a game state *)
type t

(** The list of players participating in the game *)
type players = Player.t list

type phase = 
  | AttackPhase
  | FortifyPhase
  | PlacePhase

(** The player whose turn it is in the game state *)
type current_player = Player.t

(** [init p] is the initial game state *)
val init : Player.t list -> t

(** [get_players game_state] are the players of the game state *)
val get_players : t -> Player.t list

(** [get_curr_player g] is the player of the game state *)
val get_current_player : t -> Player.t

(** [update_state game_state command] is a new game state after [command] 
    is executed
*)
val update_state : t -> Command.command -> t

(** [update_players game_state new_players] is a new game state with
    players equal to [new_players]
*)
val update_players : t -> Player.t list -> t

(** [perform_attack game_state attack_detail] is a new game state
    after attack is applied
*)
val perform_attack : t -> Command.attack_detail -> t

(** [perform_attack game_state attack_detail] is a new game state
    after attack is applied
*)
val perform_place : t -> Command.place_detail -> t

val perform_fortify : t -> Command.fortify_detail -> t