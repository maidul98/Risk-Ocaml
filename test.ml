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
let world_json = Yojson.Basic.from_file "worldmap.json"
let map = Map.json_to_map world_json
let alaska = map |> Map.territories |> List.hd

let player = Player.init "playerA"
let player = Player.add_territory player alaska
let player = Player.add_troops player 1

let card = Card.init "Alaska"
let card = Card.add_territory card alaska

let map_tests =
  [

  ]

let territory_name_test
    (description : string)
    (territory : Territory.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.name alaska))

let territory_owner_test
    (description : string)
    (territory : Territory.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.owner alaska))

let territory_troops_test
    (description : string)
    (territory : Territory.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Territory.count alaska)
        ~printer:string_of_int)

let territory_neighbors_test
    (description : string)
    (territory : Territory.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Territory.neighbors alaska))

let territory_tests =
  [
    territory_name_test "prints Alaska" alaska "Alaska";
    territory_owner_test "prints none" alaska "None";
    territory_troops_test "prints 1" alaska 0;
    territory_neighbors_test "prints playerA's neighbors list" alaska
      ["Kamchatka"; "Northwest Territory"; "Alberta"];
  ]

let card_name_test
    (description : string)
    (card : Card.t)
    (expected_output : string) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Card.name card))

let terr_to_str_lst terr =
  List.map (fun x -> Territory.name x) terr

let card_valid_locations_test
    (description : string)
    (card : Card.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (terr_to_str_lst (Card.valid_locs card)))

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
      assert_equal expected_output (Player.name player))

let player_troops_test
    (description : string)
    (player : Player.t)
    (expected_output : int) : test =
  description >:: (fun _ ->
      assert_equal expected_output (Player.count player)
        ~printer:string_of_int)

let player_territories_test
    (description : string)
    (player : Player.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (terr_to_str_lst (Player.territories player)))

let player_add_territory_test
    (description : string)
    (player : Player.t)
    (expected_output : 'a list) : test =
  description >:: (fun _ ->
      let p_new = Player.add_territory player alaska in
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (terr_to_str_lst (Player.territories p_new)))

(* let player_styles_test
    (description : string)
    (player : Player.t)
    (expected_output : 'a list) : test =
   description >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_output (Player.styles player)) *)

let player_tests =
  [
    player_name_test "prints playerA" player "playerA";
    player_troops_test "prints 1" player 1;
    player_territories_test "prints ['Alaska']" player ["Alaska"];
    player_add_territory_test "prints ['Alaska'; 'Alaska']" player
      ["Alaska"; "Alaska"];
    (* player_styles_test "prints playerA's styles" player
       [Bold; Background(Red)]; *)
  ]

let suite =
  "test suite for Risk-OCaml" >::: List.flatten [
    map_tests;
    territory_tests;
    card_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite
