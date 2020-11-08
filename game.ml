type players = Player.t list

type current_player = Player.t

(* type phase =
   | Attackify
   | Fortify
   | Place *)

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

(* create two lists of dice and compare to see how many troops the defense lost
 * pads with zeros if list lengths aren't equal to three *)
let dice_results num_offense_dies num_defense_dies =
  let rec get_dies num lst =
    match num with
    | 0 -> if List.length lst = 1 then lst @ [0] @ [0] else if List.length lst = 2 then lst @ [0] else lst
    | _ -> get_dies (num - 1) (lst @ [(1 + Random.int 6)])
  in
  let offense_dies = get_dies num_offense_dies [] in
  let defense_dies = get_dies num_defense_dies [] in
  List.fold_left (fun x y -> x + y) 0 (List.map2 (fun o d -> if o > d then 1 else 0) offense_dies defense_dies)

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

(* convert command class to list with same information *)
(* let get_command_info terr =
   match (terr : Command.command) with
   | Attack a -> [a.from_country; a.to_country]
   | Place p -> [string_of_int p.count; p.country]
   | Fortify f -> [string_of_int f.count; f.from_country; f.to_country]
   | _ -> [] *)

(* note: this function will work once other files are updated to return the appropriate states
 * (e.g. offense & defense variables currently store strings but need to be Territory.t objects) *)
(* let attack game_state terr =
   Random.self_init (); (* move this to where game state is initialized since it shouldn't be called more than once *)
   let get_info = get_command_info terr in
   let offense = List.nth get_info 0 in
   let defense = List.nth get_info 1 in
   let o_troops = Territory.get_count offense in
   let d_troops = Territory.get_count defense in
   let get_dice_nums = dice_nums o_troops d_troops in
   let get_dice_res = dice_results (fst get_dice_nums) (snd get_dice_nums) in
   let new_offense = Territory.set_count offense o_troops -(fst get_dice_res) in
   let new_defense = Territory.set_count defense d_troops -(fst get_dice_res) in
   game_state *)
(* update game_state correctly with new_offense as the new offense territory
 * and new_defense as the new defense territory *)

(* let update_state current_state (command : Command.command) =
   match command with
   | Attack a -> attack current_state command
   | Quit -> current_state
   | Skip -> current_state
   | _ -> current_state *)

(** [territories_from_players] given a list of [players] will return all 
    territories from all players into a single list *)
let territories_from_players players = 
  players |> List.map Player.get_territories |> List.concat


(**[get_territory_by_name] Given a name  of a territory [name] and list of 
   [territories], will return territory with the name that matches [name]*)
let rec get_territory_by_name name territories = match territories with 
  | [] -> failwith "Name not found"
  | h::t -> if Territory.get_name h = name then h 
    else get_territory_by_name name t




let place state (command : Command.command) = 
  match command with 
  | Place {count; trr_name} -> 
    let territory = state 
                    |> get_players 
                    |> territories_from_players 
                    |> get_territory_by_name trr_name in 
    Territory.set_count territory count; state

  | _ -> failwith "Logic gone wrong - place"



