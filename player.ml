type player_name = string

type player_style = ANSITerminal.style list

type troop_count = int

type territories = Territory.t list

type t = {
  name: player_name;
  troops: troop_count;
  territories: territories;
  styles: player_style;
}

let init name bg_color = {
  name = name;
  troops = 0;
  territories = [];
  styles = [Bold; bg_color];
}

let name player = player.name

let count player = player.troops

let add_troops troops_add player = {
  player with troops = player.troops + troops_add
}

let territories player = player.territories

let add_territory territory_add player = {
  player with territories = territory_add :: player.territories
}

let styles player = player.styles
