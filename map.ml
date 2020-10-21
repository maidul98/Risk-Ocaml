open Yojson.Basic.Util

type territories = Territory.t list

type regions = Region.t list

type t = {
  regions : regions;
  territories : territories
}

let get_territories_from_region_json region = region |> member "territories" |> to_list

(* List.map get_territories_from_region_json; *)
let json_to_map json = {
  regions = json |> member "regions" |> to_list |> List.map Region.init;
  territories = json |> member "regions" |> to_list |> List.map get_territories_from_region_json |> List.concat |> List.map Territory.init
}

let territories map  = map.territories





