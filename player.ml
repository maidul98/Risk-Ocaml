type player_name = string

type player_style = ANSITerminal.style list 

type territories = Territory.t list

type t = {
  name: player_name;
  styles: player_style;
  territories: territories
}

let init name = {
  name = name;
  styles = [Bold;Background(Red)];
  territories = []
}

let name player = player.name

let territories player = player.territories

let add_territory player territory_add = 
  {player with territories = territory_add::player.territories}

let styles player = player.styles
