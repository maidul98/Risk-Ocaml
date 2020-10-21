type territory_name = string

exception UnknownTerritory of territory_name

type territory_region = string

type territory_owner = string

type troop_count = int

type territory_neighbors = territory_name list


type t = {
  terr_n: territory_name; 
  region: territory_region;
  neighbors: territory_name list;
  troops: troop_count;
  owner: territory_owner
}

(*

let name territory =
  territory.terr_n

let region territory =
  territory.region

let neighbors territory =
  territory.neighbors

let owner territory =
  territory.owner

let troop_count territory =
  territory.troops

(*let get_color territory = territory.color*)

*)