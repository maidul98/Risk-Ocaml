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

let get_name player = player.name

let get_count player = player.troops

let get_territories player = player.territories

let get_styles player = player.styles

let get_cards player = player.cards

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

let check_ownership territory player =
  player
  |> get_territories
  |> List.mem territory

let check_regions player =
  let terr_lst = player
                 |> get_territories
  in
  let asia = List.filter (fun t ->
      let terr_name = Territory.get_name t
      in
      match terr_name with
      | "Kamchatka"
      | "Yakutsk"
      | "Irkutsk"
      | "Mongolia"
      | "Siberia"
      | "Ural"
      | "Japan"
      | "China"
      | "Kazakhstan"
      | "Middle East"
      | "Siam"
      | "India" -> true
      | _ -> false
    ) terr_lst
  in
  let namerica = List.filter (fun t ->
      let terr_name = Territory.get_name t
      in
      match terr_name with
      | "Alaska"
      | "Northwest_Terr"
      | "Greenland"
      | "Alberta"
      | "Ontario"
      | "Quebec"
      | "Western_US"
      | "Eastern_US"
      | "Central_America" -> true
      | _ -> false
    ) terr_lst
  in
  let europe = List.filter (fun t ->
      let terr_name = Territory.get_name t
      in
      match terr_name with
      | "Iceland"
      | "Britain"
      | "W_Europe"
      | "S_Europe"
      | "N_Europe"
      | "Scandinavia"
      | "Ukraine" -> true
      | _ -> false
    ) terr_lst
  in
  let africa = List.filter (fun t ->
      let terr_name = Territory.get_name t
      in
      match terr_name with
      | "North_Africa"
      | "Congo"
      | "South_Africa"
      | "Madagascar"
      | "E_Africa"
      | "Egypt" -> true
      | _ -> false
    ) terr_lst
  in
  let samerica = List.filter (fun t ->
      let terr_name = Territory.get_name t
      in
      match terr_name with
      | "Venezuela"
      | "Peru"
      | "Argentina"
      | "Brazil" -> true
      | _ -> false
    ) terr_lst
  in
  let australia = List.filter (fun t ->
      let terr_name = Territory.get_name t
      in
      match terr_name with
      | "Indonesia"
      | "W_Australia"
      | "E_Australia"
      | "Papua_New_Guinea" -> true
      | _ -> false
    ) terr_lst
  in
  let rec get_regions num lst =
    match num with
    | 0 -> lst
    | 1 ->
      if List.length asia = 12
      then get_regions (num - 1) (lst @ ["Asia"])
      else get_regions (num - 1) lst
    | 2 ->
      if List.length namerica = 9
      then get_regions (num - 1) (lst @ ["NAmerica"])
      else get_regions (num - 1) lst
    | 3 ->
      if List.length europe = 7
      then get_regions (num - 1) (lst @ ["Europe"])
      else get_regions (num - 1) lst
    | 4 ->
      if List.length africa = 6
      then get_regions (num - 1) (lst @ ["Africa"])
      else get_regions (num - 1) lst
    | 5 ->
      if List.length samerica = 4
      then get_regions (num - 1) (lst @ ["SAmerica"])
      else get_regions (num - 1) lst
    | 6 ->
      if List.length australia = 4
      then get_regions (num - 1) (lst @ ["Australia"])
      else get_regions (num - 1) lst
    | _ -> failwith "Error encountered"
  in
  get_regions 6 []

let cash_cards player =
  let num_cards = player |> get_cards in
  let rec cash_in num tot =
    if num >= 3
    then cash_in (num - 3) (tot + 3)
    else tot
  in
  let num_cashed_in = cash_in num_cards 0 in
  set_cards player (num_cards - num_cashed_in);
  num_cashed_in


let get_random_territory player =
  let territories = player.territories in
  let index = territories
              |> List.length
              |> Random.int
  in
  let actual_territory = List.nth territories index in
  actual_territory

let get_random_territory_and_other_neighbor player =
  let first_territory = get_random_territory player in
  let index = first_territory
              |> Territory.get_neighbors
              |> List.length
              |> Random.int
  in
  let neighbor = List.nth (first_territory
                           |> Territory.get_neighbors) index
  in (first_territory, neighbor)
