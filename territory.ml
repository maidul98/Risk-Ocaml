open Yojson.Basic.Util

type territory_name = string

exception UnknownTerritory of territory_name

type territory_owner = string

type troop_count = int

type territory_neighbors = territory_name list

type t = {
  name: territory_name; 
  neighbors: territory_name list;
  troops: troop_count;
  owner: territory_owner
}

let init json = {
  name = json |> member "name" |> to_string;
  neighbors = json |> member "neighbors" |> to_list |> List.map to_string;
  troops = 0;
  owner = "None";
}

let name territory = territory.name
