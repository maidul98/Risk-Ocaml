open Yojson.Basic.Util

(* type territories = string list

   type regions = region list *)



type territories = Territory.t list

type regions = Region.t list

type t = {
  regions : regions;
  territories : territories
}


(** Will parse out a single territory from [json] and initialize it *)
(* let make_territory json = {
   territory_name = json |> member "name" |> to_string;
   neighbors = json |> member "neighbors" |> to_list |> List.map to_string;
   color = [Bold;Background(Red)];
   troop_count = 0;
   owner = None
   } *)

(** Will parse our a single region from [json]*)
(* let make_region json = {
   region_name = json |> member "name" |> to_string;
   bonus = json |> member "bonus" |> to_int;
   territories = json |> member "territories" |> to_list |> List.map make_territory;
   } *)

let json_to_map json = failwith "not done"

(* {
   regions = json |> member "regions" |> to_list |> List.map make_region;
   } *)





