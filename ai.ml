let random_easy_fortify_clause ai_player = 
  let first_territory = ai_player
                        |> Player.get_random_territory_and_other_neighbor
                        |> fst
  in
  let first_territory_name = 
    first_territory
    |> Territory.get_name 
  in
  let second_territory_name = ai_player
                              |> Player.get_random_territory_and_other_neighbor
                              |> snd
  in
  if (Territory.get_count first_territory > 0) 
  then "fortify 1 " ^ first_territory_name ^ " " ^ second_territory_name 
  else "fortify 0 " ^ first_territory_name ^ " " ^ second_territory_name 

let random_easy_place_clause ai_player = 
  let territory_name = ai_player
                       |> Player.get_random_territory
                       |> Territory.get_name
  in 
  "place 1 " ^ territory_name

let random_easy_attack_clause ai_player =
  let first_territory = ai_player
                        |> Player.get_random_territory_and_other_neighbor
                        |> fst
  in
  let first_territory_name = 
    first_territory
    |> Territory.get_name 
  in
  let second_territory_name = ai_player
                              |> Player.get_random_territory_and_other_neighbor
                              |> snd
  in
  "attack " ^ first_territory_name ^ " " ^ second_territory_name




(* "place 1 " ^ (ai_player 
   |> Player.get_random_territory
   |> Player.get_name) *)