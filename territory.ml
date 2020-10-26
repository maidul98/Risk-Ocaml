open Yojson.Basic.Util

type territory_name = string

exception UnknownTerritory of territory_name

type territory_owner = string

type troop_count = int

type territory_neighbors = territory_name list

type t = {
  terr_name: territory_name;
  owner: territory_owner;
  troops: troop_count;
  neighbors: territory_neighbors;
}

let init json = {
  terr_name = json |> member "name" |> to_string;
  owner = "None";
  troops = 0;
  neighbors = json |> member "neighbors" |> to_list |> List.map to_string;
}

let get_name territory = territory.terr_name

let get_owner territory = territory.owner

let get_count territory = territory.troops

let get_neighbors territory = territory.neighbors

let set_owner territory new_owner = {
  territory with owner = new_owner
}

let set_count territory add_troops = {
  territory with troops = troops + add_troops
}
