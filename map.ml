open Yojson.Basic.Util

type territories = Territory.t list

type regions = Region.t list

type t = {
  regions : regions;
  territories : territories;
}

(** Given a json representation of a region, will return a list of json
   representations of the territories in that region *)
let get_territories_from_region_json region =
  region |> member "territories" |> to_list

let json_to_map json = {
  regions = json |> member "regions" |> to_list |> List.map Region.init;
  territories = json |> member "regions" |> to_list
                     |> List.map get_territories_from_region_json
                     |> List.concat |> List.map Territory.init;
}

let territories map = map.territories
