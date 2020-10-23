open OUnit2
open Map
open Territory
open Card
open Player

(********************************************************************
   BEGIN: Helper functions from A2
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
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
  in "[" ^ pp_elts lst ^ "]"

(********************************************************************
   END: Helper functions from A2.
 ********************************************************************)

let map_tests =
  [

  ]

let territoryA = {
  terr_name = "Alaska";
  owner = "playerA";
  troops = 1;
  neighbors = ["Kamchatka", "Northwest Territory", "Alberta"];
}

let territory_name_test
    (description : string)
    (territory : string)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.name territoryA))

let territory_owner_test
    (description : string)
    (territory : string)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.owner territoryA))

let territory_troops_test
    (description : string)
    (territory : string)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.count territoryA)
        ~printer:string_of_int)

let territory_neighbors_test
    (description : string)
    (territory : string)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Territory.neighbors territoryA))

let territory_tests =
  [
    territory_name_test "prints Alaska" territoryA "Alaska";
    territory_owner_test "prints playerA" territoryA "playerA";
    territory_troops_test "prints 1" territoryA 1;
    territory_neighbors_test "prints playerA's neighbors list" territoryA
      ["Kamchatka", "Northwest Territory", "Alberta"];
  ]

let cardA = {
  name: "Alaska";
  territories: ["Alberta"; "Great Britain"];
}

let card_name_test
    (description : string)
    (card : string)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Card.name card))

let card_valid_locations_test
    (description : string)
    (card : string)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Card.valid_locs card))

let card_tests =
  [
    card_name_test "prints Alaska" cardA "Alaska";
    card_valid_locations_test "prints ['Alberta'; 'Great Britain']" cardA
      ["Alberta"; "Great Britain"];
  ]

let playerA = {
  name = "playerA";
  troops = 1;
  territories = ["Alaska"];
  styles = [Bold; Background(Red)];
}

let player_name_test
    (description : string)
    (player : string)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.name player))

let player_troops_test
    (description : string)
    (player : string)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.count player)
        ~printer:string_of_int))

let player_territories_test
    (description : string)
    (player : string)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Player.neighbors player))

let player_add_territory_test
    (description : string)
    (player : string)
    (territory : string)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output ((Player.neighbors player) @ [player]))

let player_styles_test
    (description : string)
    (player : string)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Player.styles player))

let player_tests =
  [
    player_name_test "prints playerA" playerA "playerA";
    player_troops_test "prints 1" playerA 1;
    player_territories_test "prints ['Alaska']" playerA ["Alaska"];
    player_add_territory_test "prints ['Alaska', 'Kamchatka']" playerA
      "Kamchatka" ["Alaska"; "Kamchatka"];
    player_styles_test "prints playerA's styles" playerA
      [Bold; Background(Red)];
  ]

let suite =
  "test suite for Risk-OCaml" >::: List.flatten [
    map_tests;
    territory_tests;
    card_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite
