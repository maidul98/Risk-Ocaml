type players = Player.t list

type current_player = Player.t

type phase =
  | Attack
  | Fortify
  | Place

type t =
  {
    players : players;
    mutable curr_player : current_player;
    mutable phase: phase;
    mutable card_inc: int;
    mutable rem_troops: int; (* remaining troops to place *)
    mutable won_some_attack: bool;
  }

let rec init players =
  let init_troops = troops_round (List.hd players) false 0 in
  {
    players = players;
    curr_player = List.hd players;
    phase = Place;
    card_inc = 0;
    rem_troops = init_troops;
    won_some_attack = false
  }

(** [troops_round p t b] calculates the number of troops that [p] should get in
    the place stage and considers if the player wants to trade [t] their cards
    in for more troops as well as card bonuses [b] *)
and troops_round player trade bonus =
  let lst = Player.get_territories player in
  let lst_len = List.length lst in
  let round_bonus = if lst_len < 12 then 3 else lst_len / 3 in (* 4 *)
  let rec region_bonus lst num =
    match lst with
    | [] -> num
    | h :: t ->
      begin
        match h with
        | "Asia" -> region_bonus t (num + 7)
        | "NAmerica" -> region_bonus t (num + 5)
        | "Europe" -> region_bonus t (num + 5)
        | "Africa" -> region_bonus t (num + 3)
        | "SAmerica" -> region_bonus t (num + 2)
        | "Australia" -> region_bonus t (num + 2)
        | _ -> region_bonus t num
      end
  in
  let card_bonus =
    if trade || Player.get_cards player >= 5
    then
      let cards = Utility.cash_cards player in
      let rec get_card_bonus num prev =
        if num = 0 then prev else get_card_bonus (num - 3) (prev + 5)
      in
      get_card_bonus cards bonus
    else 0 in 
  let region_bonus_num = region_bonus (Utility.check_regions player) 0 in
  round_bonus + region_bonus_num + card_bonus

let get_won_some_attack game_state =
  game_state.won_some_attack

let set_won_some_attack won game_state  =
  { game_state with won_some_attack = won }

let get_rem_troops game_state =
  game_state.rem_troops

let set_rem_troops game_state count =
  { game_state with rem_troops = count }

let get_players game_state =
  game_state.players

let get_curr_player game_state =
  game_state.curr_player

let get_phase game_state =
  game_state.phase

let get_string_phase phase =
  match phase with
  | Attack -> "Attack"
  | Fortify -> "Fortify"
  | Place -> "Place"

let next_player game_state =
  let curr = get_players game_state in
  let next =
    match get_players game_state with
    | h :: t -> t @ [h]
    | _ -> failwith "no players in game"
  in
  List.combine curr next
  |> List.assoc (get_curr_player game_state)

(** [player_from_territory] gets the player that the territory corresponds to
    Requires: [lst] has length 1 *)
let player_from_territory game_state terr =
  let lst = List.filter (fun p ->
      List.mem terr (Player.get_territories p)) (get_players game_state) 
  in
  List.hd lst

(* [get_terr] combines the above functions to get a territory from [game_state]
   given a [name] *)
let get_terr game_state name =
  game_state
  |> get_players
  |> Utility.territories_from_players
  |> Utility.get_territory_by_name name

(* [dice_results] creates two lists of size [num_off_dies] and [num_def_dies],
 * fills those lists with random numbers from 1 to 6 (which represent simulated
 * dice rolls), and then compares the dice rolls (i.e. values) in the lists to
 * see how many troops were lost, returning a tuple of the form
 * (offensive_troops_lost, defensive_troops_lost)
 * Note: list comparison is case dependent on the number of available dice *)
let dice_results num_off_dies num_def_dies =
  let rec get_rolls num lst =
    match num with
    | 0 -> lst
    | _ -> get_rolls (num - 1) (lst @ [(1 + Random.int 6)]) (* 1+[0,5]=[1,6] *)
  in
  let offense_dies = List.rev (List.sort compare (get_rolls num_off_dies [])) in
  let defense_dies = List.rev (List.sort compare (get_rolls num_def_dies [])) in
  let rec cmp off_lst def_lst off_lost def_lost =
    let len_tuple = (List.length off_lst, List.length def_lst) in
    if fst len_tuple = snd len_tuple
    then match (off_lst, def_lst) with
      | ([],_) -> (off_lost, def_lost)
      | (_,[]) -> (off_lost, def_lost)
      | (o :: t1, d :: t2) ->
        begin
          if o > d
          then cmp t1 t2 off_lost (def_lost + 1)
          else cmp t1 t2 (off_lost + 1) def_lost
        end
    else match len_tuple with
      | (0,_) | (_,0) -> (0,0)
      | (1,2) | (1,3) ->
        begin
          let off_val = List.hd off_lst in
          let count = List.filter (fun d -> off_val > d) def_lst in
          if List.length count > 0 then (0,1) else (1,0)
        end
      | (2,1) | (3,1) ->
        begin
          let def_val = List.hd def_lst in
          let count = List.filter (fun o -> o > def_val) off_lst in
          if List.length count > 0 then (0,1) else (1,0)
        end
      | (2,3) ->
        begin
          let off_val_1 = List.hd off_lst in
          let off_val_2 = List.nth off_lst 1 in
          let count_1 = List.filter (fun d -> off_val_1 > d) def_lst in
          let count_2 = List.filter (fun d -> off_val_2 > d) def_lst in
          let ans = if List.length count_1 > 0 then (0,1) else (1,0) in
          if List.length count_2 > 0 then (fst ans, 1 + snd ans)
          else (1 + fst ans, snd ans)
        end
      | (3,2) ->
        begin
          let def_val_1 = List.hd def_lst in
          let def_val_2 = List.nth def_lst 1 in
          let count_1 = List.filter (fun o -> o > def_val_1) off_lst in
          let count_2 = List.filter (fun o -> o > def_val_2) off_lst in
          let ans = if List.length count_1 > 0 then (1,0) else (0,1) in
          if List.length count_2 > 0 then (1 + fst ans, snd ans)
          else (fst ans, 1 + snd ans)
        end
      | _ -> (0,0)
  in cmp offense_dies defense_dies 0 0

(* [dice_nums] converts the number of troops to number of available dice,
 * making sure that any offense territory has at least 1 reserve troop (else
 * the invariant isn't true) *)
let dice_nums offense_troops defense_troops =
  match (offense_troops, defense_troops) with
  | (0,_) -> (0,0) (* cannot attack territory with 0 offense troops *)
  | (_,0) -> (0,0) (* cannot attack territory with 0 defense troops *)
  | (1,_) -> (0,0) (* cannot attack territory with 1 offense troop *)
  | (2,1) -> (1,1)
  | (2,2) -> (1,2)
  | (2,d) -> (1,3)
  | (3,1) -> (2,1)
  | (3,2) -> (2,2)
  | (3,d) -> (2,3)
  | (o,1) -> (3,1)
  | (o,2) -> (3,2)
  | (o,d) -> (3,3) (* there are at most 3 dice for a territory *)

(* [get_dice_nums] gets the number of dice based on the number of troops *)
let get_dice_nums offense defense =
  let o_troops = Territory.get_count offense in
  let d_troops = Territory.get_count defense in
  dice_nums o_troops d_troops

let uniq_terr_owner_lst game_state =
  Utility.territories_from_players (get_players game_state)
  |> List.map (fun terr -> Territory.get_owner terr)
  |> List.sort_uniq compare

(* [validate_players] is [game_state] with players that have no
   territories ejected. *)
let validate_players game_state =
  let terr_owner_lst = uniq_terr_owner_lst game_state in
  let old_players = get_players game_state in
  let updated_players = List.filter (fun player -> begin
        List.mem (Player.get_name player) terr_owner_lst
      end) old_players in
  match List.filter (fun player ->
      not (List.mem (Player.get_name player) terr_owner_lst)) old_players with
  | [] -> { game_state with players = updated_players }
  | h :: _ -> begin
      print_endline (Player.get_name h ^ " has been removed from the game");
      { game_state with players = updated_players }
    end

let rec print_map game =
  game
  |> get_players
  |> View.assoc_territories
  |> View.print_map

(* [update_terr] assigns the newly conquered territory to the offense, removes
   it from the defense, and places at least 1 troop there. At this stage, the
   invariants of the game guarantee that [off] has at least 2 troops, one to put
   in the new territory and one to keep. *)
let conquer_terr state off def off_player def_player =
  Territory.set_owner_unit def (Territory.get_owner off); (* assign new owner *)
  Territory.set_count_unit def 1; (* places at least 1 troop in new territory *)
  Territory.sub_count off 1; (* removes that 1 troop from territory *)
  Player.del_territory_unit def def_player; (* deletes territory from defense *)
  Player.add_territory_unit def off_player; (* adds territory to offense *)
  Player.update_troops def_player; (* updates total troop count for defense *)
  Player.update_troops off_player; (* updates total troop count for offense *)
  print_map state;
  if Territory.get_count off = 1 then ()
  else
    (* moves troops from attacking territory to newly conquered territory *)
    let rec get_troops num start =
      if num > -1 && num < Territory.get_count off then num
      else
        let valid =
          if not start
          then print_endline ("Invalid action: Can't add that many troops.")
          else print_endline ""
        in
        let get_int =
          let ques = "How many troops do you want to move to " in
          if Player.get_name (get_curr_player state) <> "AI"
          then read_int (print_string (ques ^ Territory.get_name def ^ "? "))
          else 1
        in
        valid; get_troops get_int false
    in
    let num_troops = get_troops (Int.min_int) true in
    Territory.add_count def num_troops;
    Territory.sub_count off num_troops;
    ()

(* [attack] runs the attack state
 * Requires:
 * [from] and [attack] are adjacent *)
let attack state from towards =
  print_endline ("Attacking from " ^ from ^ " to " ^ towards ^ ".");
  let offense = get_terr state from in
  let defense = get_terr state towards in
  let rec attack_until off def curr_state =
    let dice_counts = get_dice_nums offense defense in
    match dice_counts with
    | (0,0) -> curr_state
    | (o,d) ->
      begin
        let get_dice_res = dice_results (fst dice_counts) (snd dice_counts) in
        Territory.sub_count offense (fst get_dice_res);
        Territory.sub_count defense (snd get_dice_res);
        (* update ownership and troop movement here *)
        if Territory.get_count defense = 0 then (* case: successful attack *)
          begin
            let off_player = player_from_territory state offense in
            let def_player = player_from_territory state defense in
            conquer_terr state offense defense off_player def_player;
            curr_state |> set_won_some_attack true
          end
        else attack_until off def curr_state
      end
  in
  (attack_until offense defense state)
  |> validate_players (* eject player here *)

(* [reprompt_state] reprompts the user to enter a new [commnad] that is valid
    for the current game phase *)
let reprompt_state current_state process_state message =
  print_endline message;
  print_string "> ";
  match read_line () with
  | command -> process_state current_state (Command.parse command)

(* [place] puts [count] troops in [terr]
 * Requires:
 * [count] >= 1 and [count] < [terr].troop count *)
let place state count terr process_state =
  if count < 0 then reprompt_state state process_state
      "You cannot place negative troops" else
    let territory = get_terr state terr in
    let current_player = get_curr_player state in
    match Utility.check_ownership territory current_player with
    | true ->
      begin
        let troops_left = (get_rem_troops state) - count in
        if troops_left < 0 then reprompt_state state process_state
            "Invalid action: cannot place this number of troops"
        else if troops_left = 0 then
          begin
            print_endline ("Placing " ^ string_of_int count ^
                           " troops in " ^ terr ^ ".");
            Territory.add_count territory count;
            let state' = set_rem_troops state troops_left in
            { state' with phase = Attack }
          end
        else
          begin
            print_endline ("Placing " ^ string_of_int count ^
                           " troops in " ^ terr ^ ".");
            Territory.add_count territory count;
            set_rem_troops state troops_left
          end
      end
    | false -> reprompt_state state process_state
                 "Invalid action: not your territory"

(* [check_reachability] is whether or not [terr2] can be reached from [terr1]
    by traversing through countries owned by the same player; the
    implementation is based on depth-first search
    Requires:
    [terr2] and [terr1] are owned by the same player
*)
let check_reachability (terr1 : string) (terr2 : string) (game_state : t) =
  let visited = ref [||] in
  let reachable = ref false in
  let current_player = get_curr_player game_state in
  let rec traverse terr_name =
    let terr = get_terr game_state terr_name in
    if (String.lowercase_ascii terr_name) = (String.lowercase_ascii terr2)
    then reachable := true (* case: [terr2] is reached*)
    else
      begin (* case: [terr2] not yet reached, so we continue traversal *)
        if (Territory.get_owner terr <> Player.get_name current_player) ||
           (Array.mem terr_name !visited)
        then () (* case: different player node or already visited *)
        else
          begin
            visited := (Array.append !visited [|terr_name|]);
            let neighbors = Territory.get_neighbors terr in
            List.iter (fun neighbor ->
                traverse (String.lowercase_ascii neighbor)) neighbors
          end
      end
  in
  traverse terr1; !reachable

(* [check_card_qual game_state] adjusts the current player's card count
   If the current player won some attack during the turn, then his/her card
   count is incremented.
   Otherwise, his/her card count is not incremented
*)
let check_card_qual game_state =
  let current_player = get_curr_player game_state
  in
  match get_won_some_attack game_state with
  | false -> ()
  | true -> begin
      Player.add_card current_player;
      ()
    end

(* [fortify] moves [count] troops from [from] to [towards]
 * Requires:
 * [from] != [towards] and [count] >= 1 and [count] < [from].troop count *)
let fortify state count from towards process_state =
  let from_t = get_terr state from in
  let to_t = get_terr state towards in
  let c_player = get_curr_player state in
  let nxt_player = next_player state in
  match Utility.check_ownership from_t c_player with
  | true ->
    begin
      match Utility.check_ownership to_t c_player with
      | true ->
        begin
          if check_reachability from towards state
          then
            begin
              print_endline ("Moving " ^ string_of_int count ^
                             " troops from " ^ from ^ " to " ^ towards ^ ".");
              Territory.sub_count from_t count;
              Territory.add_count to_t count;
              if Player.get_name c_player <> "AI"
              then begin
                check_card_qual state;
                {state with phase = Place;
                            curr_player = nxt_player;
                            rem_troops = troops_round nxt_player false 0 }
                |> set_won_some_attack false
              end
              else begin
                check_card_qual state;
                state |> set_won_some_attack false
              end
            end
          else reprompt_state state process_state
              "Invalid action: territory is not reachable"
        end
      | false -> reprompt_state state process_state
                   "Invalid action: destination territory is not yours"
    end
  | false -> reprompt_state state process_state
               "Invalid action: starting territory is not yours"

let rec process_state curr_state (command : Command.command) =
  let nxt_player = next_player curr_state in
  match get_phase curr_state with
  | Place ->
    begin
      match command with
      | Place {count; trr_name} ->
        place curr_state count trr_name process_state
      | Next -> {curr_state with phase = Attack}
      | Quit -> begin
          print_endline ("Leaving the game!"); exit 0
        end
      | _ -> reprompt_state curr_state process_state
               "Invalid action: command inconsistent with phase"
    end
  | Attack ->
    begin
      match command with
      | Attack {from_trr_name; to_trr_name} -> begin
          let off = get_terr curr_state from_trr_name in
          let def = get_terr curr_state to_trr_name in
          let off_owner = Territory.get_owner off in
          let def_owner = Territory.get_owner def in
          let def_name = Territory.get_name def in
          if from_trr_name = to_trr_name
          (* attacking and defending territories are the same *)
          then reprompt_state curr_state process_state
              "Invalid action: cannot attack and defend same territory"
          else if off_owner = def_owner
          (* current player owns both the attacking and defending territories *)
          then reprompt_state curr_state process_state
              "Invalid action: you own both territories"
          else if off_owner <> Player.get_name (get_curr_player curr_state)
          (* current player doesn't own attacking territory *)
          then reprompt_state curr_state process_state
              "Invalid action: you don't own attacking territory"
          else if not (List.mem def_name (Territory.get_neighbors off))
          (* defending territory doesn't neighbor attacking territory *)
          then reprompt_state curr_state process_state
              "Invalid action: territories do not share a border"
          else attack curr_state from_trr_name to_trr_name
        end
      | Next -> {curr_state with phase = Fortify}
      | Quit -> begin
          print_endline ("Leaving the game!"); exit 0
        end
      | _ -> reprompt_state curr_state process_state
               "Invalid action in phase; command inconsistent with phase"
    end
  | Fortify ->
    begin
      match command with
      | Fortify {count; from_trr_name; to_trr_name} -> begin
          let off = get_terr curr_state from_trr_name in
          let def = get_terr curr_state to_trr_name in
          let off_owner = Territory.get_owner off in
          let def_owner = Territory.get_owner def in
          if from_trr_name = to_trr_name
          (* attacking and defending territories are the same *)
          then reprompt_state curr_state process_state
              "Invalid action: cannot fortify to and from same territory"
          else if off_owner <> Player.get_name (get_curr_player curr_state)
          (* current player doesn't own first territory *)
          then reprompt_state curr_state process_state
              "Invalid action: you don't own first territory"
          else if off_owner <> def_owner
          (* current player doesn't own second territory *)
          then reprompt_state curr_state process_state
              "Invalid action: you don't own second territory"
          else if not (count > -1 && count < Territory.get_count off)
          (* wrong number of troops to fortify *)
          then reprompt_state curr_state process_state
              "Invalid action: cannot fortify that many troops"
          else fortify curr_state count from_trr_name to_trr_name process_state
        end
      | Next -> begin
          check_card_qual curr_state;
          {curr_state with phase = Place;
                           curr_player = nxt_player;
                           rem_troops = troops_round nxt_player false 0 }
          |> set_won_some_attack false
        end
      | Quit -> begin
          print_endline ("Leaving the game!"); exit 0
        end
      | _ -> reprompt_state curr_state process_state
               "Invalid action in phase; command inconsistent with phase"
    end

let get_num_terr_owned game_state =
  let current_player = get_curr_player game_state in
  let check_territory terr =
    Territory.get_owner terr = Player.get_name current_player
  in
  get_players game_state
  |> Utility.territories_from_players
  |> List.filter check_territory
  |> List.length

let check_game_finish game_state =
  let terr_owner_lst = uniq_terr_owner_lst game_state in
  if List.length terr_owner_lst = 1 then true else false
