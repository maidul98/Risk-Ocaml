exception No_Fortify
exception No_Attack

(** [territories_from_players] given a list of [players] will return all
    territories from all players into a single list *)
let territories_from_players players =
  players
  |> List.map Player.get_territories
  |> List.concat

(** [get_territory_by_name] Given a name of a territory [name] and list of
    [territories], will return territory with the name that matches [name] *)
let rec get_territory_by_name name territories =
  match territories with
  | [] -> failwith "Name not found"
  | h :: t ->
    begin
      if (String.lowercase_ascii (Territory.get_name h)) =
         (String.lowercase_ascii name)
      then h
      else get_territory_by_name name t
    end

let is_my_territory player territory_name =
  List.exists (fun terr -> territory_name = Territory.get_name terr) 
    (Player.get_territories player)

let isnt_my_territory player territory_name =
  List.for_all (fun terr -> territory_name <> Territory.get_name terr) 
    (Player.get_territories player)

let create_assoc territory valid_neighbors =
  List.map (fun neighbor -> territory, neighbor) valid_neighbors

let get_random_territory player =
  let territories = Player.get_territories player in
  let index = territories |> List.length |> Random.int in
  let actual_territory = List.nth territories index in
  actual_territory

let get_random_territory_and_my_neighbor player =
  let territories = Player.get_territories player in
  let all_pairs = List.concat (List.map (fun terr ->
      create_assoc terr (List.filter (is_my_territory player) 
                           (Territory.get_neighbors terr))
    ) territories) in
  if List.length all_pairs > 0 then
    let index = all_pairs |> List.length |> Random.int in
    List.nth (all_pairs) index
  else raise No_Fortify

let get_random_territory_and_other_neighbor player =
  let territories = Player.get_territories player in
  let all_pairs = List.concat (List.map ( fun terr ->
      create_assoc terr (List.filter (isnt_my_territory player) 
                           (Territory.get_neighbors terr))
    ) territories) in
  if List.length all_pairs > 0 then
    let index = all_pairs |> List.length |> Random.int in
    List.nth (all_pairs) index
  else raise No_Attack

let check_ownership territory player =
  player
  |> Player.get_territories
  |> List.mem territory

let cash_cards player =
  let num_cards = player |> Player.get_cards in
  let rec cash_in num tot =
    if num >= 3 then cash_in (num - 3) (tot + 3) else tot
  in
  let num_cashed_in = cash_in num_cards 0 in
  Player.set_cards player (num_cards - num_cashed_in);
  num_cashed_in

let check_regions player =
  let terr_lst = player |> Player.get_territories in
  let asia = List.filter (fun t ->
      let terr_name = Territory.get_name t in
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
      | "Middle_East"
      | "Siam"
      | "India" -> true
      | _ -> false
    ) terr_lst
  in
  let namerica = List.filter (fun t ->
      let terr_name = Territory.get_name t in
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
      let terr_name = Territory.get_name t in
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
      let terr_name = Territory.get_name t in
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
      let terr_name = Territory.get_name t in
      match terr_name with
      | "Venezuela"
      | "Peru"
      | "Argentina"
      | "Brazil" -> true
      | _ -> false
    ) terr_lst
  in
  let australia = List.filter (fun t ->
      let terr_name = Territory.get_name t in
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
