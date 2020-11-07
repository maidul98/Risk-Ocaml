type attack_phrase = { from_country: string; to_country: string }
type place_phrase = { count: int; country: string }
type fortify_phrase = { count: int; from_country: string; to_country: string }

(** Raised when a malformed command is encountered. *)
exception Malformed of string

(** Raised when an empty command is encountered. *)
exception Empty

(** The type [command] represents a player command *)
type command = 
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Quit
  | Skip

(** [parse str] parses a player's input into an option [command] *)
val parse : string -> command option
