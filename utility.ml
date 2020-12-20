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

(** [is_my_territory p n] checks if player [p] owns territory with name [n] *)
let is_my_territory player territory_name =
  List.exists (fun terr -> territory_name = Territory.get_name terr)
    (Player.get_territories player)

(** [isnt_my_territory p n] checks if player [p] doesn't own territory with
    name [n] *)
let isnt_my_territory player territory_name =
  List.for_all (fun terr -> territory_name <> Territory.get_name terr)
    (Player.get_territories player)

(** [create_assoc t n] creates an association map of type ([t], neighbors of
    [n]) *)
let create_assoc territory valid_neighbors =
  List.map (fun neighbor -> territory, neighbor) valid_neighbors

(** [get_random_territory p] gets a random territory of player [p] *)
let get_random_territory player =
  let territories = Player.get_territories player in
  let index = territories |> List.length |> Random.int in
  let actual_territory = List.nth territories index in
  actual_territory

(** [get_ai_fortify p] runs the fortify step for the AI [p] *)
let get_ai_fortify player =
  let territories = Player.get_territories player in
  let all_pairs = List.concat (List.map (fun terr ->
      create_assoc terr (List.filter (is_my_territory player)
                           (Territory.get_neighbors terr))
    ) territories) in
  if List.length all_pairs > 0 then
    let index = all_pairs |> List.length |> Random.int in
    List.nth (all_pairs) index
  else raise No_Fortify

(** [get_ai_attack p] runs the attack step for the AI [p] *)
let get_ai_attack player =
  let territories = Player.get_territories player in
  let all_pairs = List.concat (List.map ( fun terr ->
      create_assoc terr (List.filter (isnt_my_territory player)
                           (Territory.get_neighbors terr))
    ) territories) in
  if List.length all_pairs > 0 then
    let index = all_pairs |> List.length |> Random.int in
    List.nth (all_pairs) index
  else raise No_Attack

(** [check_ownership t p] checks if player [p] owns territory [t] *)
let check_ownership territory player =
  player
  |> Player.get_territories
  |> List.mem territory

(** [cash_cards p] trades in the cards that player [p] owns for more troops *)
let cash_cards player =
  let num_cards = player |> Player.get_cards in
  let rec cash_in num tot =
    if num >= 3 then cash_in (num - 3) (tot + 3) else tot
  in
  let num_cashed_in = cash_in num_cards 0 in
  Player.set_cards player (num_cards - num_cashed_in);
  num_cashed_in

(** [check_regions_asia t] checks if all territories in Asia are in [t] *)
let check_regions_asia terr_lst =
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
  if List.length asia = 12 then ["Asia"] else []

(** [check_regions_namerica t] checks if all territories in North America are
    in [t] *)
let check_regions_namerica terr_lst =
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
  if List.length namerica = 9 then ["NAmerica"] else []

(** [check_regions_europe t] checks if all territories in Europe are in [t] *)
let check_regions_europe terr_lst =
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
  if List.length europe = 7 then ["Europe"] else []

(** [check_regions_africa t] checks if all territories in Africa are in [t] *)
let check_regions_africa terr_lst =
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
  if List.length africa = 6 then ["Africa"] else []

(** [check_regions_samerica t] checks if all territories in South America are
    in [t] *)
let check_regions_samerica terr_lst =
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
  if List.length samerica = 4 then ["SAmerica"] else []

(** [check_regions_australia t] checks if all territories in Australia are in
    [t] *)
let check_regions_australia terr_lst =
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
  if List.length australia = 4 then ["Australia"] else []

(** [check_regions p] checks what regions player [p] owns all territories in *)
let check_regions player =
  let terr_lst = player |> Player.get_territories in
  let asia = check_regions_asia terr_lst in
  let namerica = check_regions_namerica terr_lst in
  let europe = check_regions_europe terr_lst in
  let africa = check_regions_africa terr_lst in
  let samerica = check_regions_samerica terr_lst in
  let australia = check_regions_australia terr_lst in
  asia @ namerica @ europe @ africa @ samerica @ australia
