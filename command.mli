type object_phrase = string list

(** Raised when a malformed command is encountered. *)
exception Malformed

(** The type [command] represents a player command *)
type command =
  | Action of object_phrase
  | Quit

(** [parse str] parses a player's input into an option [command] *)
val parse : string -> command option