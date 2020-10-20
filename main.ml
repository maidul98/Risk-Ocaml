let main () =
  let map = Map.json_to_map (Yojson.Basic.from_file "worldmap.json") in 
  View.print_map (Map.all_territories_assoc map)

(* Execute the game*)
let () = main ()