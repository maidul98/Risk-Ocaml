(** The representation for a game state *)
type t

(** The current round of the game *)
type round = int

(** The list of players participating in the game *)
type players = Player.t list

(** The player whose turn it is at the current game state *)
type player_turn = Player.t

(** [init p] is the initial game state *)
val init : Player.t list -> t

(** [perform_round g] is the new game state after game state [g] *)
val perform_round : t -> t