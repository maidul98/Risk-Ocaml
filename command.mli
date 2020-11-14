type attack_phrase = { from_trr_name: string; to_trr_name: string }
type place_phrase = { count: int; trr_name: string }
type fortify_phrase = { count: int; from_trr_name: string; to_trr_name: string }

(** Raised when a malformed command is encountered. *)
exception Malformed of string

(** Raised when an empty command is encountered. *)
exception Empty of string

(** The type [command] represents a player command *)
type command =
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Next

(** [parse str] parses a player's input into an option [command] *)
val parse : string -> command
