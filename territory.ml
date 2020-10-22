open Yojson.Basic.Util

type territory_name = string

exception UnknownTerritory of territory_name

type territory_owner = string

type troop_count = int

type territory_neighbors = territory_name list

type t = {
  name: territory_name;
  owner: territory_owner;
  troops: troop_count;
  neighbors: territory_neighbors;
}

let init json = {
  name = json |> member "name" |> to_string;
  owner = "None";
  troops = 0;
  neighbors = json |> member "neighbors" |> to_list |> List.map to_string;
}

let name territory = territory.name

let owner territory = territory.owner

let count territory = territory.troops

let neighbors territory = territory.neighbors
