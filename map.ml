open Yojson.Basic.Util

type t = {name : map_name; territories : Territory.t list}

let make_territory json = {
  name = json |> member "name" |> to_string;
  neighbors = json |> member "neighbors" |> to_list;
}

let make_region json = {
  name = json |> member "name" |> to_string;
  bonus = json |> member "bonus" |> to_int;
  exits = json |> member "territories" |> to_list |> List.map make_territory;
}

let from_json json = {
  name = json |> member "name" |> to_string;
  territories = json |> member "regions" |> to_list |> List.map make_region;
}
