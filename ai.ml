let random_easy_fortify_clause ai_player =
  try
    let pair_of_territories = ai_player
                              |> Player.get_random_territory_and_my_neighbor
    in
    let first_territory = pair_of_territories |> fst in
    let first_territory_name = first_territory |> Territory.get_name in
    let second_territory_name = pair_of_territories |> snd in
    if (Territory.get_count first_territory > 0)
    then "fortify 1 " ^ first_territory_name ^ " " ^ second_territory_name
    else "fortify 0 " ^ first_territory_name ^ " " ^ second_territory_name
  with x -> raise x

let random_easy_place_clause ai_player =
  let territory_name = ai_player
                       |> Player.get_random_territory
                       |> Territory.get_name
  in "place 1 " ^ territory_name

let random_easy_attack_clause ai_player =
  try
    let pair_of_territories = ai_player
                              |> Player.get_random_territory_and_other_neighbor
    in
    let first_territory = pair_of_territories |> fst in
    let first_territory_name = first_territory |> Territory.get_name in
    let second_territory_name = pair_of_territories |> snd in
    "attack " ^ first_territory_name ^ " " ^ second_territory_name
  with x -> raise x