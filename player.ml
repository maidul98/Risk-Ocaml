type player_name = string

type player_style = ANSITerminal.style list

type troop_count = int

type territories = Territory.t list

type t =
  {
    name: player_name;
    mutable troops: troop_count;
    mutable territories: territories;
    styles: player_style;
    mutable cards: int;
  }

let init name bg_color =
  {
    name = name;
    troops = 0;
    territories = [];
    styles = [Bold; bg_color];
    cards = 0;
  }

let get_name player =
  player.name

let get_count player =
  player.troops

let get_territories player =
  player.territories

let get_styles player =
  player.styles

let get_cards player =
  player.cards

let add_troops troops_add player =
  {
    player with troops = player.troops + troops_add
  }

let update_troops player =
  player.troops <- List.fold_left (fun x terr ->
      x + Territory.get_count terr) 0 player.territories

let add_territory territory_add player =
  {
    player with territories = territory_add :: player.territories
  }

let add_territory_unit territory_add player =
  player.territories <- territory_add :: player.territories

let del_territory_unit territory_del player =
  player.territories <- List.filter (fun terr ->
      terr <> territory_del) player.territories

let add_card player =
  player.cards <- player.cards + 1

let set_cards player num =
  player.cards <- num

(*
let first_territory = get_random_territory player in
let index = first_territory
            |> Territory.get_neighbors
            |> List.length
            |> Random.int
in
let neighbor = List.nth (first_territory
                         |> Territory.get_neighbors) index
in (first_territory, neighbor) *)
