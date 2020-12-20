open Ai

let json = "worldmap.json"
let file = Yojson.Basic.from_file json

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
    ANSITerminal.Background (Red)
  ]
  in
  let rec go color_lst initialized =
    function
    | [] -> initialized
    | h1 :: t1 ->
      begin
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
    List.sort (fun _ _ -> (Random.int 3) - 1) territories
  in
  let rec go players_new =
    function
    | [] -> players_new
    | h1 :: t1 ->
      begin (* h1 := territory we wish to assign *)
        match players_new with
        | [] -> failwith "Precondition Violation"
        | h2 :: t2 ->
          begin (* h2 := player we wish to assign h1 *)
            let owned_h1 = (Territory.set_owner h1 (Player.get_name h2)) in
            let player_assigned = Player.add_territory owned_h1 h2 in
            go (t2 @ [player_assigned]) t1
          end
      end
  in
  go players shuffled_territories

(** [assign_troops players] will assign troop counts to each territory such that
    all players start with an equal number of troops
    Requires:
    [players] length is greater than 0
*)
let assign_troops players =
  let num_players = List.length players in
  let num_troops_per_player =
    if num_players = 2 then 40
    else if num_players = 3 then 35
    else if num_players = 4 then 30
    else if num_players = 5 then 25
    else if num_players = 6 then 20
    else 15
  in
  List.map (fun player ->
      let terr_lst = Player.get_territories player in
      let terr_lst_ln = List.length terr_lst in
      (* shuffle [terr_lst] like in [assign_territories] *)
      let terr_lst_2 = List.sort (fun _ _ -> Random.int terr_lst_ln) terr_lst in
      (* go through [terr_lst_2] and add troops 1 by 1 until none left *)
      let rec place_troops lst orig_lst troops_left =
        match troops_left with
        | 0 -> player
        | num ->
          begin
            match lst with
            | [] -> place_troops orig_lst orig_lst troops_left
            | terr :: tail -> begin
                Territory.add_count terr 1;
                place_troops tail orig_lst (troops_left - 1)
              end
          end
      in
      place_troops terr_lst_2 terr_lst_2 num_troops_per_player
    ) players

(** [get_players] will ask the user for player information and return a list of
    [player]s *)
let get_players has_ai =
  let num_players = read_int
      (print_string "How many players do you want in the game? ") in
  let rec get_names num lst =
    if num > num_players then lst
    else
      begin
        let name_prompt = print_string
            ("What is player " ^ (string_of_int num) ^ "'s name? ") in
        let name = read_line name_prompt in
        print_string
          ("> Player " ^ (string_of_int num) ^ "'s name is " ^ name ^ ".\n");
        get_names (num + 1) lst @ [name]
      end
  in
  if has_ai then init_players ("AI":: (get_names 1 []))
  else init_players (get_names 1 [])

(** [print_map g] prints the game g*)
let rec print_map game =
  game
  |> Game.get_players
  |> View.assoc_territories
  |> View.print_map

(** [get_curr_player g] get the current player in game g*)
let get_curr_player game =
  game
  |> Game.get_curr_player

(** [get_curr_name g] get the name of the current player in game g*)
let get_curr_name game =
  game
  |> get_curr_player
  |> Player.get_name

(** [get_curr_style g] get the style of the current player in game g*)
let get_curr_style game =
  game
  |> get_curr_player
  |> Player.get_styles

(** [get_curr_phase g] get the phase of game g*)
let get_curr_phase game =
  game
  |> Game.get_phase
  |> Game.get_string_phase

let example_attack = 
  "Attack Example: attack <from territory name> <to territory name> or enter
  'next' or 'quit'" 
let example_place = 
  "Place Example: place <# troops to place> <territory name> or enter
  'next' or 'quit'"
let example_fortify = 
  "Fortify Example: 
  fortify <# troops to move> <from territory name> <to territory name> or enter
  'next' or 'quit'"

(** [get_example g] returns the appropriate example for game g*)
let get_example game =
  let rem_troops = string_of_int (Game.get_rem_troops game)
  in
  match Game.get_phase game with
  | Game.Attack -> print_endline example_attack
  | Game.Place ->
    begin
      print_endline ("Remaining troops to place: " ^ rem_troops);
      print_endline example_place
    end
  | Game.Fortify -> print_endline example_fortify

(** [handle_ai_place g] returns the updated game after handling the 
    place phase of the ai in game g *)
let handle_ai_place game =
  let game_one = Game.process_state game 
      (Command.parse (random_easy_place_clause (Game.get_curr_player game))) in
  let game_two = Game.process_state game_one (Command.parse ("next")) in
  print_map game_two; game_two

(** [handle_ai_attack g] returns the updated game after handling the 
    attack phase of the ai in game g *)
let handle_ai_attack game =
  try
    let game_one = Game.process_state game 
        (Command.parse (random_easy_attack_clause 
                          (Game.get_curr_player game))) in
    let game_two = Game.process_state game_one (Command.parse ("next")) in
    print_map game_two; game_two
  with _ ->
    let game_two = Game.process_state game (Command.parse ("next")) in
    print_map game_two; game_two

(** [handle_ai_fortify g] returns the updated game after handling the 
    fortify phase of the ai in game g *)
let handle_ai_fortify game =
  try
    let game_one = Game.process_state game 
        (Command.parse (random_easy_fortify_clause 
                          (Game.get_curr_player game))) in
    let game_two = Game.process_state game_one (Command.parse ("next")) in
    print_map game_two; game_two
  with _ ->
    let game_two = Game.process_state game (Command.parse ("next")) in
    print_map game_two; game_two

(** [prompt_cash_cards g] return the game_state after handling the 
    player's cash cards *)
let rec prompt_cash_cards game_state =
  let current_player = Game.get_curr_player game_state in
  let num_cards_owned = Player.get_cards current_player in
  let cash_msg = {|
    You have cashable cards. Would you like to cash your cards for additional
    troops? (yes/no)
    |}
  in
  if num_cards_owned >= 3
  then begin
    print_endline cash_msg;
    match read_line () with
    | command ->
      match String.lowercase_ascii command with
      | "yes" ->
        let troops_for_round = Game.troops_round current_player true 0 in
        Game.set_rem_troops game_state troops_for_round
      | "no" -> game_state
      | _ -> begin
          print_endline "Let's try that again";
          prompt_cash_cards game_state
        end
  end
  else game_state

(** [play g] plays the game g *)
let rec play game =
  let current_player = Game.get_curr_player game in
  let game =
    if Game.get_phase game = Game.Place &&
       Player.get_name current_player <> "AI"
    then prompt_cash_cards game
    else game
  in
  match Game.check_game_finish game with
  | true ->
    begin
      print_endline ("Congratulations " ^ get_curr_name game ^
                     ". You have conquered the world!"); exit 0
    end
  | false ->
    begin
      let num_terr_owned = Game.get_num_terr_owned game in
      let num_cards_owned = Player.get_cards current_player in
      print_map game;
      if get_curr_name game = "AI"
      then
        let after_place = handle_ai_place game in
        let after_attack = handle_ai_attack after_place in
        let after_fortify = handle_ai_fortify after_attack in
        play after_fortify;
      else
        ANSITerminal.(print_string (game |> get_curr_style)
                        ("It's " ^ (game |> get_curr_name) ^ "'s turn.\n"));
      print_endline ("Current phase is: " ^ (game |> get_curr_phase));
      print_endline 
        ("Number of territories owned: " ^ string_of_int num_terr_owned);
      print_endline ("Number of cards owned: " ^ string_of_int num_cards_owned);
      get_example game;
      print_string "> ";
      match read_line () with
      | command ->
        begin
          match Command.parse command with
          | t -> play (Game.process_state game (t))
          | exception (Command.Empty m) -> print_endline m; play game
          | exception (Command.Malformed m) -> print_endline m; play game
          | exception (Command.Negative_int m) -> print_endline m; play game
        end
    end

(** [make_ai_player] initalises the ai *)
let make_ai_player =
  Player.init "AI" (ANSITerminal.Background (Red))

let info =
  let welcome_msg = {|
Welcome to Risk in OCaml! Here's how the game works:

You are the commander of an army of troops and you have to conquer the world.
You will be randomly assigned initial territories and provided with an initial
number of troops. Each round, you will be given an extra number of troops to
help you attack other territories (owned by other players). If you succeed, the
territory becomes yours. Conquer all territories and you will rule the world!

Good luck, warrior!
|}
  in
  print_endline welcome_msg;
  print_string "Would you like to include an AI? ";
  match read_line () with
  | command -> if String.lowercase_ascii command = "yes" then true else false

let rec main () =
  Random.self_init ();
  let territories = file
                    |> Map.json_to_map
                    |> Map.get_territories
  in
  let players = get_players info
                |> assign_territories territories
                |> assign_troops
  in
  play (Game.init players)

(* Execute the game *)
let () = main ()
