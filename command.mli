(** This module represents abstract commands, as well as handles the parsing of
    string information from the user into moves in a Risk game. *)

(** Record of data associated with attacking *)
type attack_phrase = {
  from_trr_name: string;
  to_trr_name: string
}

(** Record of data associated with place *)
type place_phrase = {
  count: int;
  trr_name: string
}

(** Record of data associated with fortify *)
type fortify_phrase = {
  count: int;
  from_trr_name: string;
  to_trr_name: string
}

(** Raised when a malformed command is encountered *)
exception Malformed of string

(** Raised when an empty command is encountered *)
exception Empty of string

(** Raised when the user inputs a negative value in command *)
exception Negative_int of string

(** The type [command] represents a player command *)
type command =
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Next
  | Quit

(** [parse str] parses a player's input into a [command] *)
val parse : string -> command

(** [parse_attack] given a token list [tokens], it will return an
    [attack_phrase] command *)
val parse_attack : string list -> attack_phrase

(** [parse_place] given a token list [tokens], it will return a
    [place_phrase] command *)
val parse_place : string list -> place_phrase

(** [parse_fortify] given a token list [tokens], it will return a
    [fortify_phrase] command *)
val parse_fortify : string list -> fortify_phrase
