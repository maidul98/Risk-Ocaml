open Yojson.Basic.Util

type territories = Territory.t list

type regions = Region.t list

type t = 
  {
    regions: regions;
    territories: territories;
  }

(** [get_territories_from_region_json] is the list of json territories
    corresponding to [region].
    Requires:
    [region] is a valid json representation for a territory
*)
let get_territories_from_region_json region =
  region 
  |> member "territories" 
  |> to_list

let json_to_map json = 
  {
    regions = json 
              |> member "regions" 
              |> to_list 
              |> List.map Region.init;
    territories = json
                  |> member "regions"
                  |> to_list
                  |> List.map get_territories_from_region_json
                  |> List.concat
                  |> List.map Territory.init
  }

let get_territories map = map.territories

let get_regions map = map.regions

(* [get_territory] is the Territory.t version of [terr] in [map]
   Requires: [terr] is a valid territory in [map] *)
let get_territory map terr =
  List.hd (List.filter (fun t -> Territory.get_name t = terr) map.territories)
