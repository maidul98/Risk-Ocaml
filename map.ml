open Yojson.Basic.Util

(** Eventually change these to match the type t in the other files *)
type territory = {territory_name: string; neighbors: string list}
type region = {region_name: string; bonus: int; territories: territory list}
type t = {map_name : string; regions : region list}

let make_territory json = {
  territory_name = json |> member "name" |> to_string;
  neighbors = json |> member "neighbors" |> to_list |> List.map to_string
}

let make_region json = {
  region_name = json |> member "name" |> to_string;
  bonus = json |> member "bonus" |> to_int;
  territories = json |> member "territories" |> to_list |> List.map make_territory;
}

let from_json json = {
  map_name = json |> member "name" |> to_string;
  regions = json |> member "regions" |> to_list |> List.map make_region;
}

(** Capitalize first letter of each word *)
let capitalize_letters str =
  let split = String.split_on_char ' ' str in
  let cap = List.map (fun x -> String.capitalize_ascii x) split in
  String.concat " " cap

let file = Yojson.Basic.from_file "worldmap.json"
let worldmap = from_json file

let get_name m =
  m.map_name

let get_regions m =
  m.territories

(** Eventually change below to something like x.region_name = r.region_name since x, r will
    be type t objects *)
let get_region m r =
  List.find (fun x -> x.region_name = capitalize_letters r) m.regions

let get_region_bonus m r =
  (get_region m r).bonus

let get_territories m r =
  (get_region m r).territories

(** Eventually change below to something like x.territory_name = r.territory_name since x, r will
    be type t objects *)
let get_territory m r t =
  List.find (fun x -> x.territory_name = capitalize_letters t) (get_territories m r)

let get_neighbors m r t =
  (get_territory m r t).neighbors

(** Eventually change below to something like x.name = n.name since x, n will
    be type t objects *)
let get_neighbor m r t n =
  List.find (fun x -> x = capitalize_letters n) (get_neighbors m r t)
