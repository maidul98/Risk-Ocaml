let file = Yojson.Basic.from_file "worldmap.json"

(** [init_players players] is [players] but initialized and with 
    different colors 
    Requires:
    [players] length is between 1...4 inclusive
*)
let init_players players =
  let color_lst = [
    ANSITerminal.Background (Blue);
    ANSITerminal.Background (Magenta); 
    ANSITerminal.Background (Green);
    ANSITerminal.Background (Red)] in
  let rec go color_lst initialized = function
    | [] -> initialized
    | h1 :: t1 -> begin
        match color_lst with
        | [] -> failwith "More Colors Required"
        | h2 :: t2 -> go t2 (Player.init h1 h2 :: initialized) t1
      end
  in
  go color_lst [] players

(** [assign_territories territories players] is a list of players with
    the [territories] randomly partitioned amongst them 
    Requires:
    [players] length is greater than 0
*)
let assign_territories territories players =
  let shuffled_territories = 
    List.sort (fun _ _ -> (Random.int 3) - 1) territories in
  let rec go players_new = function
    | [] -> players_new
    | h1 :: t1 -> begin (* h1 := territory we wish to assign *)
        match players_new with
        | [] -> failwith "Precondition Violation"
        | h2 :: t2 -> begin (* h2 := player we wish to assign h1 *)
            let player_assigned = Player.add_territory h1 h2 in
            go (t2 @ [player_assigned]) t1
          end
      end
  in
  go players shuffled_territories

let main () =
  let territories = file |> Map.json_to_map |> Map.territories in
  let players = init_players ["Kunal"; "Maidul"; "Tony"; "Vedant"] in
  players 
  |> assign_territories territories 
  |> View.assoc_territories 
  |> View.print_map

(* Execute the game *)
let () = main ()