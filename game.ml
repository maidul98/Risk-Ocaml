type round = int

type players = Player.t list

type player_turn = Player.t

type t = {
  round : round;
  players : players;
  player_turn : player_turn;
}

let init players = {
  round = 1;
  players = players;
  player_turn = List.hd players
}