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
  in go color_lst [] players

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
            let owned_h1 = (Territory.set_owner h1 (Player.get_name h2)) in
            let player_assigned = Player.add_territory owned_h1 h2 in
            go (t2 @ [player_assigned]) t1
          end
      end
  in go players shuffled_territories

(** [get_players] will ask the user for player information and return a list of
    [player]s *)
let get_players =
  let num_players = read_int (print_string "How many players do you want? ") in
  let rec get_names num lst =
    if num > num_players then lst
    else begin
      let name_prompt = print_string ("What is player " ^ (string_of_int num) ^ "'s name? ") in
      let name = read_line name_prompt in
      print_string ("> Player " ^ (string_of_int num) ^ "'s name is " ^ name ^ ".\n");
      get_names (num + 1) lst @ [name]
    end in
  init_players (get_names 1 [])

let print_map game =
  game |> Game.get_players |> View.assoc_territories |> View.print_map

let get_curr_player game =
  game |> Game.get_current_player

let get_curr_name game =
  game |> get_curr_player |> Player.get_name

let get_curr_style game =
  game |> get_curr_player |> Player.get_styles

let get_curr_phase game =
  game |> Game.get_phase |> Game.get_string_phase

let rec play game =
  print_map game;
  ANSITerminal.(print_string (game |> get_curr_style) ("It's " ^ (game |> get_curr_name) ^ "'s turn.\n"));
  print_endline ("Current phase is: " ^ (game |> get_curr_phase));
  print_string "> ";
  match read_line () with
  | command -> begin
      match Command.parse command with
      | t -> play (Game.process_state game (t))
      | exception (Command.Empty m) -> print_endline m; play game
      | exception (Command.Malformed m) -> print_endline m; play game
    end



let main () =
  Random.self_init (); (* ensures numbers are more random instead of pseudo-random *)
  let territories = file |> Map.json_to_map |> Map.get_territories in
  let players = get_players |> assign_territories territories in
  play (Game.init players)

(* Execute the game *)
let () = main ()
