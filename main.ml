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
            let player_assigned = Player.add_territory h1 h2 in
            go (t2 @ [player_assigned]) t1
          end
      end
  in go players shuffled_territories


(** [ask_for_players] will return a list of [player]s once the user has
    finished adding players*)
let rec ask_for_players player_number players = 
  print_endline ("Please add " ^ "player number " ^ string_of_int player_number  ^ " then hit enter.\n");
  print_string "> ";
  match read_line () with
  | "" -> init_players players
  | player_name -> print_endline ("Player number " ^ string_of_int player_number ^ " has been added"); ask_for_players (player_number+1) (player_name::players)


let print_map player territories = failwith ""

(* * [read_command] will take in user input and return a [command] *)
(* let read_commanddddd = match read_line () with 
   | command -> Command.parse command  *)

let get_current_player_style game = game |> Game.get_current_player |> Player.get_styles


let rec main_game game = 
  ANSITerminal.(print_string (game |> get_current_player_style) ("It's " ^ Player.get_name(Game.get_current_player game) ^"'s turn" ));
  print_endline "";
  print_endline ("Current phase is: " ^ (game |> Game.get_phase |> Game.get_string_phase));
  print_string "> ";
  match read_line () with
  | command -> main_game (Game.process_state game (Command.parse command))

let main () =
  let territories = file |> Map.json_to_map |> Map.get_territories in
  let players = ask_for_players 1 [] |> assign_territories territories in 
  players |> View.assoc_territories |> View.print_map; main_game (Game.init players)

(* Execute the game *)
let () = main ()