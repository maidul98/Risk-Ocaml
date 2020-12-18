open Yojson.Basic.Util

type regions

type name = string

type t = {
  region_name: name;
  bonus: int;
  territories: string list;
}

(** [get_territory_name_from_json] is the name of the territory corresponding
    to [json].
    Requires:
    [json] is a valid json representation for a territory
*)
let get_territory_name_from_json json = json
                                        |> member "name"
                                        |> to_string

let init json = {
  region_name = json
                |> member "name"
                |> to_string;
  bonus = json
          |> member "bonus"
          |> to_int;
  territories = json
                |> member "territories"
                |> to_list
                |> List.map get_territory_name_from_json;
}

let get_region_name region =
  region.region_name

let get_bonus region =
  region.bonus

let get_territories region =
  region.territories
