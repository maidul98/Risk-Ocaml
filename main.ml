let file = Yojson.Basic.from_file "worldmap.json"

let main () =
  let territories = file |> Map.json_to_map |> Map.territories in
  let p1 = Player.init "Maidul" in
  let newP = Player.add_territory p1 (List.hd territories) in
  let terr_assoc = View.assoc_territories [newP] in View.print_map terr_assoc

(* Execute the game *)
let () = main ()
