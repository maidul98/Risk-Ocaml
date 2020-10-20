open Yojson.Basic.Util

(** Eventually change these to match the type t in the other files *)
type territory = {
  territory_name: string; 
  neighbors: string list;
  color: ANSITerminal.style list;
  troop_count: int;
  owner: string option
}

type region = {
  region_name: string; 
  bonus: int; 
  territories: territory list
}

type regions = region list

type t = {
  regions : region list
}

type territories_assoc = (string * territory) list

(** Will parse out a single territory from [json] and initialize it *)
let make_territory json = {
  territory_name = json |> member "name" |> to_string;
  neighbors = json |> member "neighbors" |> to_list |> List.map to_string;
  color = [Bold;Background(Red)];
  troop_count = 0;
  owner = None
}

(** Will parse our a single region from [json]*)
let make_region json = {
  region_name = json |> member "name" |> to_string;
  bonus = json |> member "bonus" |> to_int;
  territories = json |> member "territories" |> to_list |> List.map make_territory;
}

let json_to_map json = {
  regions = json |> member "regions" |> to_list |> List.map make_region;
}


let rec territories_to_assoc (territories : territory list ) = 
  match territories with
  | [] -> []
  | h::t -> (h.territory_name, h)::territories_to_assoc t


let rec territories_from_regions regions =
  match regions with 
  | [] -> []
  | h::t -> (territories_to_assoc h.territories) :: territories_from_regions t

(** [get_all_territories regions] will return an assoc list where the key is 
    the name of the territory and the value is of type [territory] 
    Example: [("USA", { territory_name: string; neighbors: string list; 
    color: ANSITerminal.style list; troop_count: int; owner: string option })]*)
let all_territories_assoc map = 
  List.concat (territories_from_regions map.regions)

let get_color territory = territory.color

