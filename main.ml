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

(* runs the attack phase once *)
(* Note: Game.t is a placeholder for the game state variable *)
let run_phase =
  let inform_attack = print_string "You are in the attack phase. Please enter who you wish to attack.\nEx: attack 'alaska' 'alberta' will use alaska to attack alberta. Enter 'quit' when complete.\n"
  in let get_attack = read_line inform_attack in
  let get_attack_info = Command.parse get_attack in
  (* let new_game_state = Game.update_state Game.t get_attack_info in *)
  get_attack_info (* uncomment above line once Game.init is assigned *)

(* runs the attack phase until player wishes to move on *)
(* Note: Game.t is a placeholder for the game state variable *)
let attack_turns territories player =
  let player_n = print_string ("It is now " ^ Player.get_name player ^ "'s turn.\n") in
  let rec run_attack attack_count =
    let attack_result = run_phase in
    if attack_result = Quit then attack_count else run_attack (attack_count + 1)
  in
  let attack_tot = print_string ("You attacked" ^ (string_of_int (run_attack 1)) ^ "territories.\n") in
  let fortify_n = print_string ("Fortify a territory you can reach.\n") in
  let get_fortify = read_line fortify_n in
  let get_fortify_info = Command.parse get_fortify in
  (* Game.update_state Game.t get_fortify_info *)
  get_fortify_info (* replace this with above line once Game.init is assigned *)

(* gets the game settings from the user and prints them back for verification *)
let get_settings =
  let prompts = ["Welcome to Risk in OCaml! How many players do you want? ";
                 "Would you like to randomly assign territories (enter 0) or choose territories (enter 1)? ";
                 "Would you like to have a progressive card bonus (enter 0) or a fixed card bonus (enter 1)? ";
                 "Would you like to have an AI?\nEnter 0 for no, 1 for beginner, 2 for easy, 3 for medium, 4 for hard, or 5 for expert. ";
                 "Would you like to have fog of war on (enter 0) or off (enter 1)? ";
                 "Would you like to have blizzards on (enter 0) or off (enter 1)? ";
                 "Would you like to have dice rolls that are balanced blitz (enter 0) or true random (enter 1)? "] in
  let settings = [["randomly assign territories"; "choose territories"];
                  ["progressive card bonus"; "fixed card bonus"];
                  ["no AI"; "beginner AI"; "easy AI"; "medium AI"; "hard AI"; "expert AI"];
                  ["fog on"; "fog off"];
                  ["blizzards on"; "blizzards off"];
                  ["balanced blitz dice rolls"; "true random dice rolls"]] in
  let get_info = List.map (fun x -> read_int (print_string x)) prompts in
  let num_players = List.hd get_info in
  let rec get_player_names num lst =
    if num > num_players then lst
    else begin
      let player_name_prompt = print_string ("What is player " ^ (string_of_int num) ^ "'s name? ") in
      let player_name = read_line player_name_prompt in
      let print_player_name = print_string ("Player " ^ (string_of_int num) ^ "'s name is " ^ player_name ^ ".\n") in
      get_player_names (num + 1) lst @ [player_name]
    end in
  let get_names = get_player_names 1 [] in
  let players = init_players get_names in
  let print_summary = print_string ("You have selected " ^ (string_of_int num_players) ^ " players and the following options:\n") in
  List.map2 (fun s i -> print_string ((List.nth s i) ^ "\n")) settings (List.tl get_info)

let main () =
  let territories = file |> Map.json_to_map |> Map.get_territories in
  let players = init_players ["Kunal"; "Maidul"; "Tuan"; "Vedant"] in
  players
  |> assign_territories territories
  |> View.assoc_territories
  |> View.print_map

(* Execute the game *)
let () = main ()
