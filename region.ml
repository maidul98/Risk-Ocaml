open Yojson.Basic.Util
type regions

type name = string

type t = {
  region_name: name; 
  bonus: int; 
  territories: string list
}

(**Given a json representation of a single territory, will return the 
   territory name*)
let get_territory_name_from_json json = member "name" json |> to_string

let init json = {
  region_name = json |> member "name" |> to_string;
  bonus = json |> member "bonus" |> to_int;
  territories = json |> member "territories" |> to_list |> List.map get_territory_name_from_json;
}

