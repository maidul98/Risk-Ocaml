type players = Player.t list

type current_player = Player.t

type phase =
  | Attack
  | Fortify
  | Place

type t = {
  players : players;
  mutable curr_player : current_player;
  mutable phase: phase
}

let init players = {
  players = players;
  curr_player = List.hd players;
  phase = Place
}

let get_players game_state = game_state.players

let get_current_player game_state = game_state.curr_player

let get_phase game_state = game_state.phase

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
    | _ -> failwith "no players in game" in
  List.combine curr next |> List.assoc (get_current_player game_state)

(** [territories_from_players] given a list of [players] will return all
    territories from all players into a single list *)
let territories_from_players players =
  players |> List.map Player.get_territories |> List.concat

(** [get_territory_by_name] Given a name of a territory [name] and list of
    [territories], will return territory with the name that matches [name] *)
let rec get_territory_by_name name territories =
  match territories with
  | [] -> failwith "Name not found"
  | h::t -> if ( String.lowercase_ascii (Territory.get_name h)) = (String.lowercase_ascii name) then h
    else  get_territory_by_name name t

(* [get_terr] combines the above functions to get a territory from [game_state]
   given a [name] *)
let get_terr game_state name =
  get_territory_by_name name (territories_from_players (get_players game_state))

(* [dice_results] creates two lists of size [num_offense_dies] and [num_defense_dies],
 * fills those lists with random numbers from 1 to 6 (which represent simulated
 * dice rolls), and then compares the dice rolls (i.e. values) in the lists to
 * see how many troops were lost, returning a tuple of the form
 * (offensive_troops_lost, defensive_troops_lost)
 * Note: the list comparison is case dependent on the number of available dice *)
let dice_results num_offense_dies num_defense_dies =
  let rec get_rolls num lst =
    match num with
    | 0 -> lst
    | _ -> get_rolls (num - 1) (lst @ [(1 + Random.int 6)])
  in
  let offense_dies = List.rev (List.sort compare (get_rolls num_offense_dies [])) in
  let defense_dies = List.rev (List.sort compare (get_rolls num_defense_dies [])) in
  let rec cmp off_lst def_lst off_lost def_lost =
    let len_tuple = (List.length off_lst, List.length def_lst) in
    if fst len_tuple = snd len_tuple
    then match (off_lst, def_lst) with
      | ([],_) -> (off_lost, def_lost)
      | (_,[]) -> (off_lost, def_lost)
      | (o :: t1, d :: t2) -> begin
          if o > d then cmp t1 t2 off_lost (def_lost + 1)
          else cmp t1 t2 (off_lost + 1) def_lost
        end
    else match len_tuple with
      | (0,_) -> (0,0)
      | (_,0) -> (0,0)
      | (1,3) -> begin
          let off_val = List.hd off_lst in
          let count = List.filter (fun d -> off_val > d) def_lst in
          if List.length count > 0 then (0,1) else (1,0)
        end
      | (2,1) -> begin
          let def_val = List.hd def_lst in
          let count = List.filter (fun o -> o > def_val) off_lst in
          if List.length count > 0 then (0,1) else (1,0)
        end
      | (2,3) -> begin
          let off_val_1 = List.hd off_lst in
          let off_val_2 = List.nth off_lst 1 in
          let count_1 = List.filter (fun d -> off_val_1 > d) def_lst in
          let count_2 = List.filter (fun d -> off_val_2 > d) def_lst in
          let ans = if List.length count_1 > 0 then (0,1) else (1,0) in
          if List.length count_2 > 0 then (fst ans, 1 + snd ans) else (1 + fst ans, snd ans)
        end
      | _ -> (0,0)
  in cmp offense_dies defense_dies 0 0

(* [dice_nums] converts the number of troops to number of available dice,
 * making sure that any offense territory has at least 1 reserve troop (else it
 * can't protect its own territory) *)
let dice_nums offense_troops defense_troops =
  match (offense_troops, defense_troops) with
  | (0,_) -> (0,0) (* cannot attack territory with 0 offense troops *)
  | (_,0) -> (0,0) (* cannot attack territory with 0 defense troops *)
  | (1,_) -> (0,0) (* cannot attack territory with 1 offense troop *)
  | (2,1) -> (1,1)
  | (2,2) -> (1,1)
  | (2,d) -> (1,3)
  | (3,1) -> (2,1)
  | (3,2) -> (2,1)
  | (3,d) -> (2,3)
  | (o,d) -> (3,3) (* there are at most 3 dice for a territory *)

(* [get_dice_nums] gets the number of dice based on the number of troops *)
let get_dice_nums offense defense =
  let o_troops = Territory.get_count offense in
  let d_troops = Territory.get_count defense in
  dice_nums o_troops d_troops

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
    | (o,d) -> begin
        let get_dice_res = dice_results (fst dice_counts) (snd dice_counts) in
        Territory.sub_count offense (fst get_dice_res);
        Territory.sub_count defense (snd get_dice_res);
        attack_until off def curr_state
      end
  in attack_until offense defense state

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
  let territory = get_terr state terr in
  let current_player = get_current_player state in
  match Player.check_ownership territory current_player with
  | true -> begin
      print_endline ("Placing " ^ string_of_int count ^ " troops in " ^ terr ^ ".");
      Territory.add_count territory count; state
    end
  | false -> reprompt_state state process_state "Invalid action: not your territory"

(* [check_reachability] is whether or not [terr2] can be reached from [terr1]
    by traversing through countries owned by the same player; the 
    implementation is based on depth-first search
    Requires:
    [terr2] and [terr1] are owned by the same player
*)
let check_reachability (terr1 : string) (terr2 : string) (game_state : t) =
  let visited = ref [||] in (* reference to keep track of visited nodes *)
  let reachable = ref false in 
  let current_player = get_current_player game_state in
  let rec traverse terr_name =
    let terr = get_terr game_state terr_name in 
    if (String.lowercase_ascii terr_name) = (String.lowercase_ascii terr2) then reachable := true (* case: [terr2] is reached*)
    else begin (* case: [terr2] not yet reached, so we continue traversal *)
      if (Territory.get_owner terr <> Player.get_name current_player) || (Array.mem terr_name !visited) then () (* case: different player node or already visited *)
      else begin
        visited := (Array.append !visited [|terr_name|]);
        let neighbors = Territory.get_neighbors terr in
        List.iter (fun neighbor -> traverse (String.lowercase_ascii neighbor)) neighbors
      end
    end
  in
  traverse terr1; !reachable

(* [fortify] moves [count] troops from [from] to [towards]
 * Requires:
 * [from] != [towards] and [count] >= 1 and [count] < [from].troop count *)
let fortify state count from towards process_state =
  let from_t = get_terr state from in
  let to_t = get_terr state towards in
  let c_player = get_current_player state in
  match Player.check_ownership from_t c_player with
  | true -> begin
      match Player.check_ownership to_t c_player with
      | true -> begin
          if check_reachability from towards state then begin
            print_endline ("Moving " ^ string_of_int count ^ " troops from " ^ from ^ " to " ^ towards ^ ".");
            Territory.sub_count from_t count; Territory.add_count to_t count; state
          end
          else reprompt_state state process_state "Invalid action: territory is not reachable"
        end
      | false -> reprompt_state state process_state "Invalid action: destination territory is not yours"
    end
  | false -> reprompt_state state process_state "Invalid action: starting territory is not yours"

(* [process_state] is the new game state based on [current_state] and [command] *)
let rec process_state current_state (command : Command.command) =
  match get_phase current_state with
  | Place -> begin match command with
      | Place {count; trr_name} -> place current_state count trr_name process_state
      | Next -> {current_state with phase = Attack}
      | _ -> reprompt_state current_state process_state "Invalid action: command inconsistent with phase"
    end
  | Attack -> begin match command with
      | Attack {from_trr_name; to_trr_name} -> attack current_state from_trr_name to_trr_name
      | Next -> {current_state with phase = Fortify}
      | _ -> reprompt_state current_state process_state "Invalid action in phase; command inconsistent with phase"
    end
  | Fortify -> begin match command with
      | Fortify {count; from_trr_name; to_trr_name} -> fortify current_state count from_trr_name to_trr_name process_state
      | Next -> {current_state with phase = Place; curr_player = next_player current_state}
      | _ -> reprompt_state current_state process_state "Invalid action in phase; command inconsistent with phase"
    end