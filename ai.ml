(** [random_easy_fortify_clause p] creates a fortify clause for the player p *)
let random_easy_fortify_clause ai_player =
  try
    let pair_of_territories = ai_player
                              |> Utility.get_ai_fortify
    in
    let first_territory = pair_of_territories
                          |> fst in
    let first_territory_name = first_territory
                               |> Territory.get_name in
    let second_territory_name = pair_of_territories
                                |> snd in
    if (Territory.get_count first_territory > 1)
    then "fortify 1 " ^ first_territory_name ^ " " ^ second_territory_name
    else "fortify 0 " ^ first_territory_name ^ " " ^ second_territory_name
  with x -> raise x

(** [random_easy_place_clause p] creates a place clause for the player p *)
let random_easy_place_clause ai_player =
  let territory_name = ai_player
                       |> Utility.get_random_territory
                       |> Territory.get_name
  in "place 1 " ^ territory_name

(** [random_easy_attack_clause p] creates a attack clause for the player p *)
let random_easy_attack_clause ai_player =
  try
    let pair_of_territories = ai_player
                              |> Utility.get_ai_attack
    in
    let first_territory = pair_of_territories
                          |> fst in
    let first_territory_name = first_territory
                               |> Territory.get_name in
    let second_territory_name = pair_of_territories
                                |> snd in
    "attack " ^ first_territory_name ^ " " ^ second_territory_name
  with x -> raise x
