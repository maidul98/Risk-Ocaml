type round = int

type players = Player.t list

type curr_player = Player.t

type t = {
  round : round;
  players : players;
  curr_player : curr_player;
}

let init players = {
  round = 1;
  players = players;
  curr_player = List.hd players
}

let get_round game = game.round

let get_curr_player game = game.curr_player