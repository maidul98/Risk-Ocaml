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

(** [assign_troops players] will assign troop counts to each territory such
    that all players start with an equal number of troops
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
      let terr_lst_len = List.length terr_lst in
      (* shuffle [terr_lst] like in [assign_territories] *)
      let terr_lst_2 = List.sort (fun _ _ -> Random.int terr_lst_len) terr_lst in
      (* go through [terr_lst_2] and keep adding troops 1 by 1 until none left *)
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
let get_players has_ai=
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

let rec print_map game =
  game
  |> Game.get_players
  |> View.assoc_territories
  |> View.print_map

let get_curr_player game =
  game
  |> Game.get_current_player

let get_curr_name game =
  game
  |> get_curr_player
  |> Player.get_name

let get_curr_style game =
  game
  |> get_curr_player
  |> Player.get_styles

let get_curr_phase game =
  game
  |> Game.get_phase
  |> Game.get_string_phase

let example_attack = "Attack Example: attack <from territory name> <to territory name>"
let example_place = "Place Example: place <# troops to place> <territory name>"
let example_fortify = "Fortify Example: fortify <# troops to move> <from territory name> <to territory name>"

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


let rec play game =
  match Game.check_game_finish game with
  | true -> print_endline ("Congratulations " ^ get_curr_name game ^
                           ". You have conquered the world!"); exit 0
  | false ->
    begin
      let num_terr_owned = string_of_int (Game.get_num_terr_owned game)
      in
      print_map game;
      if get_curr_name game = "AI" then
        let game_one = Game.process_state game (Command.parse (random_easy_place_clause (Game.get_current_player game))) in
        let game_two = Game.process_state game_one (Command.parse ("next")) in
        print_map game_two;
        try 
          let game_three = Game.process_state game_two (Command.parse (random_easy_attack_clause (Game.get_current_player game))) in
          let game_four = Game.process_state game_three (Command.parse ("next")) in
          print_map game_four;
          let game_five = Game.process_state game_four (Command.parse (random_easy_fortify_clause (Game.get_current_player game))) in
          let game_six = Game.process_state game_five (Command.parse ("next")) in
          print_map game_six;
          play game_six
        with _ -> 
          let game_four = Game.process_state game_two (Command.parse ("next")) in 
          print_map game_four;
          try
            let game_five = Game.process_state game_four (Command.parse (random_easy_fortify_clause (Game.get_current_player game))) in
            let game_six = Game.process_state game_five (Command.parse ("next")) in
            print_map game_six;
            play game_six
          with _ -> let game_six = Game.process_state game_four (Command.parse ("next")) in
            play game_six 

      (* try 
         let game_three = Game.process_state game_two (Command.parse (random_easy_attack_clause (Game.get_current_player game))) in
         let game_four = Game.process_state game_three (Command.parse ("next")) in
         print_map game_four;
         let game_five = Game.process_state game_four (Command.parse (random_easy_fortify_clause (Game.get_current_player game))) in
         let game_six = Game.process_state game_five (Command.parse ("next")) in
         print_map game_six;
         with
         | Player.No_Attack -> 
         begin 
          let game_four = Game.process_state game_two (Command.parse ("next")) in 
          print_map game_four;
          let game_five = Game.process_state game_four (Command.parse (random_easy_fortify_clause (Game.get_current_player game))) in
          let game_six = Game.process_state game_five (Command.parse ("next")) in
          print_map game_six;
         end
         | Player.No_Fortify -> 
         begin 

          let game_six = Game.process_state game_four (Command.parse ("next")) in
          play game_six

         end  *)


      else
        ANSITerminal.(print_string (game
                                    |> get_curr_style)
                        ("It's " ^ (game
                                    |> get_curr_name) ^ "'s turn.\n"));
      print_endline ("Current phase is: " ^ (game
                                             |> get_curr_phase));
      print_endline ("Number of territories owned: " ^ num_terr_owned);
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


let make_ai_player = Player.init "AI" (ANSITerminal.Background (Red))

let ai =
  print_string "Would you like to include an AI? ";
  match read_line () with
  | command -> if command = "yes" then true else false

let rec main () =
  Random.self_init ();
  let territories = file
                    |> Map.json_to_map
                    |> Map.get_territories
  in
  let players = get_players ai
                |> assign_territories territories
                |> assign_troops
  in
  play (Game.init players)
(* Execute the game *)
let () = main ()
