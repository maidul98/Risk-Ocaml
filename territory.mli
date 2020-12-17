(** The abstract representation of a territory *)
type t

(** A territory name *)
type territory_name = string

(** The name of the owner of this territory *)
type territory_owner = string

(** The number of troops in this territory *)
type troop_count = int

(** The list of the territory's neighbors' names *)
type territory_neighbors = territory_name list

(** [init json] is the territory represented by [json] *)
val init : Yojson.Basic.t -> t

(** [get_name t] is the name of [t] *)
val get_name : t -> territory_name

(** [get_owner t] is the name of the owner of [t] *)
val get_owner : t -> territory_owner

(** [get_count t] is the troop count stationed in [t] *)
val get_count : t -> troop_count

(** [get_neighbors t] are the names of the territories neighboring [t] *)
val get_neighbors : t -> territory_neighbors

(** [set_owner t o] is [t] but with the name of its owner set to [o] *)
val set_owner : t -> territory_owner -> t

(** [add_count t c] is [t] but with [c] added to its existing troop count *)
val set_owner_unit : t -> territory_owner -> unit

(** [set_count t c] is [t] but with its troop count set to [c] *)
val set_count : t -> troop_count -> t

(** [set_count t c] is [t] but with its troop count set to [c] *)
val set_count_unit : t -> troop_count -> unit

(** [add_count t c] is [t] but with [c] added to its existing troop count *)
val add_count : t -> troop_count -> unit

(** [sub_count t c] is [t] but with [c] removed from its existing troop count *)
val sub_count : t -> troop_count -> unit
