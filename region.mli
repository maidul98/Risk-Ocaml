type t

(*
(** [terr_of_region reg] returns the list of territory names in the region*)
val terr_of_region: t -> 'a list

(** [bonus reg] returns the bonus troops a player receives if they have all the territories
    of a region *)
val bonus: t -> int

(** [name reg] returns the name of a region *)
val name: t -> string
*)
