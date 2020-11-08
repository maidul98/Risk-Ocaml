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

let get_name player = player.name

let get_count player = player.troops

let add_troops troops_add player = {
  player with troops = player.troops + troops_add
}

let get_territories player = player.territories

let add_territory territory_add player = {
  player with territories = territory_add :: player.territories
}

let remove_territory territory_remove player = 
  let player_terr = get_territories player in
  {
    player with territories = 
                  List.filter (fun terr -> terr <> territory_remove) player_terr
  }


let get_styles player = player.styles
