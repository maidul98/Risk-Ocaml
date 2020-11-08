type players = Player.t list

type current_player = Player.t

type phase = 
  | AttackPhase
  | FortifyPhase
  | PlacePhase

type t = {
  players : players;
  current_player : current_player;
}

let init players = {
  players = players;
  current_player = List.hd players
}

let get_players game = game.players

let get_current_player game = game.current_player

let update_players game_state new_players = {
  game_state with players = new_players
}

(* note: function under development to probabilistically attack territories
   - Tony
*)
let prob_attack 
    (game_state : t)
    (from_territory : string) 
    (to_territory : string)
  =
  let init_from_count = failwith "Unimplemented" in
  let init_to_count = failwith "Unimplemented" in
  let players = get_players game_state in
  let rec go from_count to_count =
    if from_count = 1 || to_count = 0 (* case: attacker/defender loses attack *)
    then failwith "Unimplented" (* data structure? *)
    else begin (* case: simulate one attack round *)
      if (Random.int 5) + 1 <= (Random.int 5) + 1
      then go (from_count - 1) (to_count) (* case: attacker loses round *)
      else go (from_count) (to_count - 1) (* case: defender loses round *)
    end
  in go init_from_count init_to_count

let update_state game_state (command : Command.command) = match command with
  | Attack attack_detail -> failwith ""
  | Place place_detail -> failwith ""
  | Fortify fortify_detail -> failwith ""
  | Quit -> failwith ""
  | Skip -> failwith ""

let perform_attack game_state (attack_detail: Command.attack_detail) =
  match game_state with
  | { players; current_player } -> begin
      match attack_detail with
      | { from_country; to_country } -> begin
          failwith "Unimplemented"
        end
    end

let perform_place game_state attack_detail = failwith "Unimplemented"

let perform_fortify game_state attack_detail = failwith "Unimplemented"