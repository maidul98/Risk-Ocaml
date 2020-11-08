type attack_detail = { from_country: string; to_country: string }
type place_detail = { count: int; country: string }
type fortify_detail = { count: int; from_country: string; to_country: string }

(** Raised when a malformed command is encountered. *)
exception Malformed of string

(** Raised when an empty command is encountered. *)
exception Empty

(** The type [command] represents a player command *)
type command = 
  | Attack of attack_detail
  | Place of place_detail
  | Fortify of fortify_detail
  | Quit
  | Skip

(** [parse str] parses a player's input into an option [command] *)
val parse : string -> command
