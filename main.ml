let file = Yojson.Basic.from_file "worldmap.json"

let main () =
  file |> Map.json_to_map |> Map.territories |> List.hd |> Territory.name |> print_string

(* View.print_map (Map.all_territories_assoc map) *)

(* Execute the game*)
let () = main ()