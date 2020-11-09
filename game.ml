type players = Player.t list

type current_player = Player.t

type phase =
  | Attackify
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

(*only works if there are at least two players in the game
  returns the next player in the game, helps with updating state*)
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
  | h::t -> if Territory.get_name h = name then h
    else get_territory_by_name name t

(* combines the above functions *)
let get_terr game_state name =
  get_territory_by_name name (territories_from_players (get_players game_state))

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
let attack game_state from towards =
  Random.self_init (); (* move this to where game state is initialized since it shouldn't be called more than once *)
  let offense = get_terr game_state from in
  let defense = get_terr game_state towards in
  let dice_numbers = get_dice_nums offense defense in
  let rec attack_until dice_counts curr_state =
    match dice_counts with
    | (0,0) -> curr_state
    | (o,d) -> begin
        let get_dice_res = dice_results (fst dice_counts) (snd dice_counts) in
        Territory.sub_count offense (fst get_dice_res);
        Territory.sub_count defense (snd get_dice_res);
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

let place state count terr = 
  let territory = get_terr state terr in 
  Territory.add_count territory count; state

let fortify state count from towards =
  let from_trr = get_terr state from in 
  let to_trr = get_terr state towards in 
  Territory.sub_count from_trr count; Territory.add_count to_trr count; state

(* determine which state to run based on the command *)
let update_state current_state (command : Command.command) =
  match command with
  | Attack {from_trr_name; to_trr_name} -> attack current_state from_trr_name to_trr_name
  | Fortify {count; from_trr_name; to_trr_name} -> fortify current_state count from_trr_name to_trr_name
  | Place {count; trr_name} -> place current_state count trr_name
  | Next -> 
    match get_phase current_state with
    | Place -> {current_state with phase = Attackify}
    | Attackify -> {current_state with phase = Fortify}
    | Fortify -> {current_state with phase = Attackify; curr_player = next_player current_state}




