(** The representation of a player *)
type t

(** The name of the player. *)
type player_name = string

(** The list of styles to repersent this player*)
type player_style = ANSITerminal.style list 

(** The total number of owned troops. *)
(* type troops = int *)

(** The list of owned territories. *)
type territories = Territory.t list

(** [name p] is the name of player [p] *)
val name : t -> player_name

(****** [init name] returns a type t with given name *******)
val init : player_name -> t

(** [troops p] is the total number of owned troops for player [p] *)
(* val count : t -> troops *)

(** [territories p] is a set-like list of all territories that player [p] holds *)
val territories : t -> territories

(** Will add a territory to the list of territories this player owns*)
val add_territory : t -> Territory.t -> t

(** Given a player, will return the styles of that player*)
val styles : t -> player_style

