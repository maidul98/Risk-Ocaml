type players = Player.t list

type current_player = Player.t

type t = {
  players : players;
  curr_player : current_player;
}

let init players = {
  players = players;
  curr_player = List.hd players
}

let get_current_player game = game.curr_player

let get_players game_state = game_state.players

(** [territories_from_players] given a list of [players] will return all
    territories from all players into a single list *)
let territories_from_players players =
  players |> List.map Player.get_territories |> List.concat

(** [get_territory_by_name] Given a name of a territory [name] and list of
    [territories], will return territory with the name that matches [name] *)
let rec get_territory_by_name name territories =
  match territories with
  | [] -> failwith "Name not found"
  | h::t -> if Territory.get_name h = name then h
    else get_territory_by_name name t

(* combines the above functions *)
let get_terr game_state name =
  get_territory_by_name name (territories_from_players (get_players game_state))

(* convert command class to list with same information *)
let get_command_info terr =
  match (terr : Command.command) with
  | Attack a -> [a.from_trr_name; a.to_trr_name]
  | Place p -> [string_of_int p.count; p.trr_name]
  | Fortify f -> [string_of_int f.count; f.from_trr_name; f.to_trr_name]
  | _ -> []

(* create two lists of dice and compare to see how many troops were lost
 * returns a tuple of the form (off_troops_lost, def_troops_lost) *)
let dice_results num_offense_dies num_defense_dies =
  let rec get_dies num lst =
    match num with
    | 0 -> lst
    | _ -> get_dies (num - 1) (lst @ [(1 + Random.int 6)])
  in
  let offense_dies = List.rev (List.sort compare (get_dies num_offense_dies [])) in
  let defense_dies = List.rev (List.sort compare (get_dies num_defense_dies [])) in
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
          let cnt = List.filter (fun d -> off_val > d) def_lst in
          if List.length cnt > 0 then (0,1) else (1,0)
        end
      | (2,1) -> begin
          let def_val = List.hd def_lst in
          let cnt = List.filter (fun o -> o > def_val) off_lst in
          if List.length cnt > 0 then (0,1) else (1,0)
        end
      | (2,3) -> begin
          let off_val_1 = List.hd off_lst in
          let off_val_2 = List.nth off_lst 1 in
          let cnt_1 = List.filter (fun d -> off_val_1 > d) def_lst in
          let cnt_2 = List.filter (fun d -> off_val_2 > d) def_lst in
          let ans = if List.length cnt_1 > 0 then (0,1) else (1,0) in
          if List.length cnt_2 > 0 then (fst ans, 1 + snd ans) else (1 + fst ans, snd ans)
        end
      | _ -> (0,0)
  in cmp offense_dies defense_dies 0 0

(* convert number of troops to number of available dice *)
let dice_nums offense_troops defense_troops =
  match (offense_troops, defense_troops) with
  | (0,_) -> (0,0)
  | (_,0) -> (0,0)
  | (1,_) -> (0,0)
  | (2,1) -> (1,1)
  | (2,2) -> (1,1)
  | (2,d) -> (1,3)
  | (3,1) -> (2,1)
  | (3,2) -> (2,1)
  | (3,d) -> (2,3)
  | (o,d) -> (3,3)

(* get number of dice based on number of troops *)
let get_dice_nums offense defense =
  let o_troops = Territory.get_count offense in
  let d_troops = Territory.get_count defense in
  dice_nums o_troops d_troops

(* run the attack state -- design decision to make on whether we want to return new game state *)
(* If IMPORTANT is true: make sure to update game_state correctly with new_offense
as the new offense territory and new_defense as the new defense territory once we
initialize game state *)
let attack game_state terr =
  Random.self_init (); (* move this to where game state is initialized since it shouldn't be called more than once *)
  let get_info = get_command_info terr in
  let offense = get_terr game_state (List.nth get_info 0) in
  let defense = get_terr game_state (List.nth get_info 1) in
  let dice_numbers = get_dice_nums offense defense in
  let rec attack_until dice_counts curr_state =
    match dice_counts with
    | (0,0) -> curr_state
    | (o,d) -> begin
      let get_dice_res = dice_results (fst dice_counts) (snd dice_counts) in
      Territory.sub_troops offense (fst get_dice_res);
      Territory.sub_troops defense (snd get_dice_res);
      attack_until (get_dice_nums offense defense) curr_state
      (* IMPORTANT: Current above line may produce infinite recursion.
       * If Territory.sub_troops modifies the offense and defense variables,
       * this should work. Otherwise, modify Territory.sub_troops, fix the new
       * game state, and then try the below commented out code: *)

      (* let new_offense = Territory.sub_troops offense (fst get_dice_res) in
      let new_defense = Territory.sub_troops defense (snd get_dice_res) in
      attack_until (get_dice_nums new_offense new_defense) curr_state *)
    end
  in attack_until dice_numbers game_state

let place state (command : Command.command) =
  match command with
  | Place {count; trr_name} ->
    let territory = state
                    |> get_players
                    |> territories_from_players
                    |> get_territory_by_name trr_name in
    Territory.set_count territory count; state
  | _ -> failwith "Logic gone wrong - place"

(* determine which state to run based on the command *)
let update_state current_state (command : Command.command) =
  match command with
  | Attack a -> attack current_state command
  (* | Fortify f -> fortify current_state command *)
  | Place p -> place current_state command
  | Quit -> current_state
  | Skip -> current_state
  | _ -> current_state
