type players = Player.t list

type current_player = Player.t

type phase = 
  | Attackify
  | Fortify
  | Place

type t = {
  players : players;
  curr_player : current_player;
}

let init players = {
  players = players;
  curr_player = List.hd players
}

let get_current_player game = game.curr_player

let update_state current_state (command : Command.command) = 
  match command with
  | Attack x -> failwith ""
  | Quit -> failwith ""
  | Skip -> failwith ""