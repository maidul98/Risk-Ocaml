open OUnit2
open Map
open Territory
open Card
open Player
open ANSITerminal
open Command
open Region
open Ai

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 
  in
  let uniq2 = List.sort_uniq compare lst2 
  in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in 
  "[" ^ pp_elts lst ^ "]"

let world_json = Yojson.Basic.from_file "worldmap.json"
let map = Map.json_to_map world_json
let alaska = map 
             |> Map.get_territories 
             |> List.hd

let greenland = List.nth (map 
                          |> Map.get_territories) 2

let alberta = List.nth (map 
                        |> Map.get_territories) 3

let ontario = List.nth (map 
                        |> Map.get_territories) 4

let north_america = map
                    |> Map.get_regions
                    |> List.hd

let asia = List.nth (map 
                     |> Map.get_regions)  4

let europe = List.nth (map 
                       |> Map.get_regions)  3

let south_america = List.nth (map 
                              |> Map.get_regions)  1

let africa = List.nth (map 
                       |> Map.get_regions)  2   

let australia = List.nth (map 
                          |> Map.get_regions)  5                                   

let player = Player.init "playerA" (ANSITerminal.Background (Red))
             |> Player.add_territory alaska 
             |> Player.add_troops 1

let playerB = Player.init "playerA" (ANSITerminal.Background (Red))
let card = Card.init "Alaska"
let card = Card.add_territory card alaska

let all_of_the_territory_names =  ["Alaska"; "Northwest_Terr"; "Greenland"; 
                                   "Alberta"; "Ontario"; "Quebec"; "Western_US";
                                   "Eastern_US"; "Central_America"; "Venezuela";
                                   "Peru"; "Argentina"; "Brazil"; 
                                   "North_Africa"; "Congo"; "South_Africa"; 
                                   "Madagascar"; "E_Africa"; "Egypt"; 
                                   "Iceland"; "Britain"; "W_Europe"; 
                                   "S_Europe"; "N_Europe"; "Scandinavia"; 
                                   "Ukraine"; "Middle_East"; "Kazakhstan"; 
                                   "Ural"; "Siberia"; "Yakutsk"; "Kamchatka"; 
                                   "Irkutsk"; "Japan"; "Mongolia"; "China"; 
                                   "India"; "Siam"; "Indonesia"; "W_Australia"; 
                                   "E_Australia"; "Papua_New_Guinea"]

let all_of_the_region_names =  ["North America"; "South America"; "Africa"; 
                                "Europe"; "Asia"; "Australia"]              


let map_get_regions_test
    (description : string)
    (map : Map.t)
    (expected_output : string list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists  
        expected_output (List.map 
                           (fun region -> Region.get_region_name region) 
                           (Map.get_regions map))
        ~printer:(pp_list pp_string))

let map_get_territories_test
    (description : string)
    (map : Map.t)
    (expected_output : string list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists  
        expected_output (List.map (fun territory -> 
            Territory.get_name territory) 
            (Map.get_territories map))
        ~printer:(pp_list pp_string))

let map_get_territory_test
    (description : string)
    (map : Map.t)
    (territory_name : string)
    (expected_count : int) 
    (expected_name : string)
    (expected_owner : string)
    (expected_neighbors : string list) 
  : test =
  description >:: (fun _ ->
      assert_equal expected_count (territory_name
                                   |> Map.get_territory map
                                   |> Territory.get_count);
      assert_equal expected_name (territory_name
                                  |> Map.get_territory map
                                  |> Territory.get_name);
      assert_equal expected_owner (territory_name
                                   |> Map.get_territory map
                                   |> Territory.get_owner);
      assert_equal expected_neighbors (territory_name
                                       |> Map.get_territory map
                                       |> Territory.get_neighbors);
    )

let map_tests =
  [
    map_get_regions_test " " map all_of_the_region_names;
    map_get_territories_test " " map all_of_the_territory_names;
    map_get_territory_test " " map "Alaska" 0 "Alaska" "None" 
      ["Kamchatka"; "Northwest_Terr"; "Alberta"];
  ]

let territory_name_test
    (description : string)
    (territory : Territory.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.get_name territory)
    )

let territory_owner_test
    (description : string)
    (territory : Territory.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.get_owner territory)
    )

let territory_troops_test
    (description : string)
    (territory : Territory.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.get_count territory)
        ~printer:string_of_int)

let territory_neighbors_test
    (description : string)
    (territory : Territory.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists 
        expected_output (Territory.get_neighbors territory)
        ~printer:(pp_list pp_string))

let territory_set_owner_test
    (description : string)
    (territory : Territory.t)
    (terr_owner: string)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output ( terr_owner 
                                     |> Territory.set_owner territory 
                                     |> Territory.get_owner)
    )

let territory_set_count_test
    (description : string)
    (territory : Territory.t)
    (troop_count: int)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (troop_count 
                                    |> Territory.set_count territory 
                                    |> Territory.get_count)
        ~printer: string_of_int )

let territory_add_count_test
    (description : string)
    (territory : Territory.t)
    (troop_count: int)
    (expected_output : unit) : test =
  description >:: (fun _ ->
      assert_equal expected_output (troop_count 
                                    |> Territory.add_count territory) 
    )

let territory_sub_count_test
    (description : string)
    (territory : Territory.t)
    (troop_count: int)
    (expected_output : unit) : test =
  description >:: (fun _ ->
      assert_equal expected_output (troop_count 
                                    |> Territory.sub_count territory) 
    )

let territory_tests =
  [
    territory_name_test "name of alaska" alaska "Alaska";
    territory_owner_test "prints none" alaska "None";
    territory_troops_test "prints 1" alaska 0;
    territory_neighbors_test "prints the list of  neighbors for the alaska 
      territory's list of neighbors" alaska
      ["Kamchatka"; "Northwest_Terr"; "Alberta"];
    territory_set_owner_test "set owner of Alaska" alaska "Me" "Me";
    territory_set_count_test "set troops of Alaska" alaska 10 10;
    territory_add_count_test " " alberta 5 ();
    territory_sub_count_test " " ontario 5 ();
    territory_name_test "name of greenland" greenland "Greenland";
    territory_owner_test "prints none" greenland "None";
    territory_troops_test "prints 1" greenland 0;
    territory_neighbors_test "prints the list of  neighbors for the greenland 
      territory's list of neighbors" greenland
      ["Northwest_Terr"; "Ontario"; "Quebec"; "Iceland"];
    territory_set_owner_test "set owner of greenland" greenland "You" "You";
    territory_set_count_test "set troops of greenland" greenland 5 5;
  ]

let card_name_test
    (description : string)
    (card : Card.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Card.get_name card)
    )

let terr_to_str_lst terr =
  List.map (fun territory -> Territory.get_name territory) terr

let card_valid_locations_test
    (description : string)
    (card : Card.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists 
        expected_output (terr_to_str_lst (Card.get_valid_locs card))
        ~printer:(pp_list pp_string))

let card_tests =
  [
    card_name_test "prints Alaska" card "Alaska";
    card_valid_locations_test "prints ['Alaska']" card
      ["Alaska"];
  ]

let player_name_test
    (description : string)
    (player : Player.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.get_name player)
    )

let player_troops_test
    (description : string)
    (player : Player.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.get_count player)
        ~printer:string_of_int)

let player_territories_test
    (description : string)
    (player : Player.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists 
        expected_output (terr_to_str_lst (Player.get_territories player))
        ~printer:(pp_list pp_string))

let player_add_territory_test
    (description : string)
    (player : Player.t ) (territory : Territory.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      let p_new = Player.add_territory territory player 
      in
      assert_equal ~cmp:cmp_set_like_lists 
        expected_output (terr_to_str_lst (Player.get_territories p_new))
        ~printer:(pp_list pp_string))

let player_styles_test
    (description : string)
    (player : Player.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists 
        expected_output (Player.get_styles player)
    )

let player_add_cards_test
    (description : string)
    (player : Player.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.get_cards player)
        ~printer:string_of_int)

let player_set_cards_test
    (description : string)
    (player : Player.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.get_cards player)
        ~printer:string_of_int)

let player_check_ownership_test
    (description : string)
    (territory) (player)
    (expected_output) : test =
  description >:: (fun _ ->
      assert_equal expected_output 
        (Player.check_ownership territory player)
    )

let player_check_regions_test
    (description : string)
    (player)
    (expected_output) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists 
        expected_output (Player.check_regions player)
    )


let alaska_with_owner = Territory.set_owner alaska "Maidul" 
let player_maidul = Player.init "Maidul" (ANSITerminal.Background (Red)) 
                    |> Player.add_territory alaska_with_owner 

let player_with_add_cards = Player.init "playerA" (ANSITerminal.Background Red)
                            |> Player.add_territory alaska 
                            |> Player.add_troops 1

let _ = Player.add_card player_with_add_cards
let _ = Player.add_card player_with_add_cards
let _ = Player.add_card player_with_add_cards

let player_with_set_cards = Player.init "playerA" (ANSITerminal.Background Red)
                            |> Player.add_territory alaska 
                            |> Player.add_troops 1

let _ = Player.set_cards player_with_set_cards 6

(* code for check regions *)
let indonesia = "playerB" 
                |> Territory.set_owner (Map.get_territory map "Indonesia")

let w_australia = "playerB" 
                  |> Territory.set_owner (Map.get_territory map "W_Australia")

let e_australia = "playerB" 
                  |> Territory.set_owner (Map.get_territory map "E_Australia")

let papua_new_guinea = "playerB" 
                       |> Territory.set_owner 
                         (Map.get_territory map "Papua_New_Guinea")

let player_owns_australia = Player.init "playerB" (ANSITerminal.Background Red) 
                            |> Player.add_territory indonesia
                            |> Player.add_territory w_australia
                            |> Player.add_territory e_australia
                            |> Player.add_territory papua_new_guinea

let player_owns_some_austr = Player.init "playerB" (ANSITerminal.Background Red) 
                             |> Player.add_territory indonesia
                             |> Player.add_territory w_australia
                             |> Player.add_territory e_australia

(** North america *)

let alaska = "playerC" 
             |> Territory.set_owner (Map.get_territory map "Alaska")

let northwest_Terr = "playerC" 
                     |> Territory.set_owner 
                       (Map.get_territory map "Northwest_Terr")

let greenland = "playerC" 
                |> Territory.set_owner 
                  (Map.get_territory map "Greenland")

let alberta = "playerC" 
              |> Territory.set_owner 
                (Map.get_territory map "Alberta")

let ontario = "playerC" 
              |> Territory.set_owner 
                (Map.get_territory map "Ontario")

let quebec = "playerC" 
             |> Territory.set_owner 
               (Map.get_territory map "Quebec")

let western_US = "playerC" 
                 |> Territory.set_owner 
                   (Map.get_territory map "Western_US")

let eastern_US = "playerC" 
                 |> Territory.set_owner 
                   (Map.get_territory map "Eastern_US")


let central_America = "playerC" 
                      |> Territory.set_owner 
                        (Map.get_territory map "Central_America")

let player_owns_north_america = Player.init "playerC" 
    (ANSITerminal.Background Red) 
                                |> Player.add_territory alaska
                                |> Player.add_territory northwest_Terr
                                |> Player.add_territory greenland
                                |> Player.add_territory alberta
                                |> Player.add_territory ontario
                                |> Player.add_territory quebec
                                |> Player.add_territory western_US
                                |> Player.add_territory eastern_US
                                |> Player.add_territory central_America

let player_owns_some_north_america = Player.init "playerC" 
    (ANSITerminal.Background Red) 
                                     |> Player.add_territory alaska
                                     |> Player.add_territory northwest_Terr
                                     |> Player.add_territory greenland
                                     |> Player.add_territory alberta
                                     |> Player.add_territory ontario
                                     |> Player.add_territory quebec
                                     |> Player.add_territory western_US
                                     |> Player.add_territory eastern_US

let owns_both_north_america_and_aus=  Player.init "playerC" 
    (ANSITerminal.Background Red) 
                                      |> Player.add_territory alaska
                                      |> Player.add_territory northwest_Terr
                                      |> Player.add_territory greenland
                                      |> Player.add_territory alberta
                                      |> Player.add_territory ontario
                                      |> Player.add_territory quebec
                                      |> Player.add_territory western_US
                                      |> Player.add_territory eastern_US
                                      |> Player.add_territory central_America
                                      |> Player.add_territory indonesia
                                      |> Player.add_territory w_australia
                                      |> Player.add_territory e_australia
                                      |> Player.add_territory papua_new_guinea


let player_owns_none_both = Player.init "playerC" 
    (ANSITerminal.Background Red) 
                            |> Player.add_territory alaska
                            |> Player.add_territory northwest_Terr
                            |> Player.add_territory greenland
                            |> Player.add_territory ontario
                            |> Player.add_territory quebec
                            |> Player.add_territory western_US
                            |> Player.add_territory eastern_US
                            |> Player.add_territory central_America
                            |> Player.add_territory indonesia
                            |> Player.add_territory w_australia
                            |> Player.add_territory papua_new_guinea
let player_tests =
  [
    player_name_test "prints playerA" player "playerA";
    player_troops_test "prints 1" player 1;
    player_territories_test "prints ['Alaska']" player ["Alaska"];

    player_add_territory_test "prints ['Greenland'; 'Alaska']" 
      player greenland ["Greenland"; "Alaska"];

    player_styles_test "player style" player [Bold; Background(Red)];
    player_add_cards_test "get number of cards" player_with_add_cards 3;
    player_set_cards_test "get number of cards" player_with_set_cards 6;

    player_check_ownership_test "check if player owns Alaska" 
      alaska_with_owner player_maidul true;

    player_check_ownership_test "check if player owns Alaska" 
      alaska_with_owner player_with_add_cards false;

    player_check_regions_test "check if player owns all of Australia" 
      player_owns_australia ["Australia"];

    player_check_regions_test "invalid: player doesn't own all Australia" 
      player_owns_some_austr [];

    player_check_regions_test "check if player owns all of North America" 
      player_owns_north_america ["NAmerica"];

    player_check_regions_test "invalid: player owns some of North America" 
      player_owns_some_north_america [];

    player_check_regions_test "player both North America and australia " 
      owns_both_north_america_and_aus ["NAmerica"; "Australia"];

    player_check_regions_test "player does not own North America or australia " 
      player_owns_none_both [];
  ]


(*REGION TESTS*)


let region_name_test
    (description : string) 
    (region : Region.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Region.get_region_name region)
    )

let region_bonus_test
    (description : string)
    (region : Region.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Region.get_bonus region)
        ~printer:string_of_int)

let region_territories_test
    (description : string) 
    (region : Region.t) 
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Region.get_territories region)
    )

let region_tests =
  [
    region_name_test "region name for region type" 
      north_america "North America";
    region_bonus_test " " north_america 5;
    region_territories_test " " north_america ["Alaska"; "Northwest_Terr"; 
                                               "Greenland"; "Alberta"; 
                                               "Ontario"; "Quebec"; 
                                               "Western_US"; "Eastern_US";
                                               "Central_America"];
    region_name_test "region name for region type" 
      asia "Asia";
    region_bonus_test " " asia 7;
    region_territories_test " " asia ["Middle_East"; "Kazakhstan"; "Ural"; 
                                      "Siberia"; "Yakutsk"; "Kamchatka"; 
                                      "Irkutsk"; "Japan"; "Mongolia"; "China"; 
                                      "India"; "Siam"];
    region_name_test "region name for region type" 
      europe "Europe";
    region_bonus_test " " europe 5;
    region_territories_test " " europe ["Iceland"; "Britain"; "W_Europe"; 
                                        "S_Europe"; "N_Europe"; "Scandinavia"; 
                                        "Ukraine"];       
    region_name_test "region name for region type" 
      south_america "South America";
    region_bonus_test " " south_america 2;
    region_territories_test " " south_america ["Venezuela"; "Peru"; 
                                               "Argentina"; "Brazil"];  
    region_name_test "region name for region type" 
      africa "Africa";
    region_bonus_test " " africa 3;
    region_territories_test " " africa ["North_Africa"; "Congo"; 
                                        "South_Africa"; "Madagascar"; 
                                        "E_Africa"; "Egypt"];  

    region_name_test "region name for region type" 
      australia "Australia";
    region_bonus_test " " australia 2;
    region_territories_test " " australia ["Indonesia"; "W_Australia"; 
                                           "E_Australia"; "Papua_New_Guinea"];                                                                         
  ]



(*COMMAND TESTS*)

let string_of_command input_command = 
  match input_command with
  | Command.Attack { from_trr_name = x; to_trr_name = y} -> 
    "attack from " ^ x ^ " to " ^ y
  | Command.Place { count; trr_name} -> 
    "place " ^ string_of_int count ^ " to " ^ trr_name
  | Command.Fortify { count; from_trr_name; to_trr_name} -> 
    "fortify " ^ string_of_int count ^ " from " ^ 
    from_trr_name ^ " to " ^ to_trr_name
  | _ -> "invalid"

let string_of_raise input_command = 
  match input_command with
  | Malformed x -> x 
  | _ -> "An error outside of malformed"

let parse_test 
    (description : string) 
    (string_command : string) 
    (expected_output) : test =
  description >:: (fun _ ->
      assert_equal expected_output 
        (string_command |> Command.parse |> string_of_command) 
        ~printer: (fun x -> x))

let parse_raise_exc_test 
    (name : string) 
    input
    (expected_output) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun x -> input |> Command.parse));;

let parse_tests = 
  [
    parse_test "place 10 to Papua_New_Guinea" "place 10 Papua_New_Guinea" 
      "place 10 to Papua_New_Guinea";
    parse_test "fortify 10 from aaa to bbb" 
      "fortify 10 Papua_New_Guinea W_Australia" 
      "fortify 10 from Papua_New_Guinea to W_Australia";

    parse_raise_exc_test "invalid place" "place x 10" 
      (Malformed "Malformed place command; please try again");

    parse_raise_exc_test "invalid fortify" "fortify x to y 10"  
      (Malformed "Malformed fortify command; please try again");
  ]

(*we need to add the empty cases for the tests *)

let all_pairs player = 
  List.map (fun terr -> (List.map (fun neighbor ->
      (Territory.get_name terr,Territory.get_name neighbor)) 
      (List.map (fun name -> Map.get_territory map name) 
         (Territory.get_neighbors terr)))) 
    (Player.get_territories player)
  |> List.concat


let ai_fortify_easy_test 
    (description : string) 
    (ai : Player.t) 
    (pairs : (Territory.territory_name * Territory.territory_name) list)
  : test =
  description >:: (fun _ ->
      let phrase = random_easy_fortify_clause ai in
      assert (List.exists (fun (first_territory, second_territory) -> 
          phrase = "fortify 1 " ^ first_territory ^ " " ^ second_territory 
          || phrase = "fortify 0 " ^ first_territory ^ " " ^ second_territory) 
          pairs)
    )

let ai_place_easy_test 
    (description : string) 
    (ai : Player.t) : test =
  description >:: (fun _ ->
      let phrase = random_easy_place_clause ai in
      assert (List.exists (fun terr -> phrase = "place 1 " ^ Territory.get_name terr) (Player.get_territories ai))
    )

let ai_attack_easy_test 
    (description : string) 
    (ai : Player.t) 
    (pairs : (Territory.territory_name * Territory.territory_name) list): test =
  description >:: (fun _ ->
      let phrase = random_easy_attack_clause ai in
      assert (List.exists (fun (first_territory, second_territory) -> 
          phrase = "attack " ^ first_territory ^ " " ^ second_territory 
          || phrase = "attack " ^ first_territory ^ " " ^ second_territory) 
          pairs)
    )

let ai_tests = 
  [
    ai_fortify_easy_test " a" player_owns_north_america (all_pairs player_owns_north_america);
    ai_place_easy_test " s" player_owns_north_america;
    ai_attack_easy_test " d" player_owns_north_america (all_pairs player_owns_north_america);
    ai_fortify_easy_test " f" player_owns_some_austr (all_pairs player_owns_some_austr);
    ai_place_easy_test " g" player_owns_some_austr;
    ai_attack_easy_test " h" player_owns_some_austr (all_pairs player_owns_some_austr);
  ]

let suite =
  "test suite for Risk-OCaml" >::: List.flatten [
    map_tests;
    territory_tests;
    card_tests;
    player_tests;
    parse_tests;
    region_tests;
    ai_tests;
  ]

let _ = run_test_tt_main suite