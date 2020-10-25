(** The representation for a game state *)
type t

(** The current round of the game *)
type round = int

(** The list of players participating in the game *)
type players = Player.t list

(** The player whose turn it is at the current game state *)
type curr_player = Player.t

(** [init p] is the initial game state *)
val init : Player.t list -> t

(** [get_round g] is the round of the game state [g] *)
val get_round : t -> int

(** [get_curr_player g] is the player of the current game state *)
val get_curr_player : t -> Player.t