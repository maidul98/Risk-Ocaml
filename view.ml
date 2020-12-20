open ANSITerminal

type player_list = Player.t list

type territory_assoc = (Territory.territory_name * Player.t) list

(** [assoc_territories_helper p] will return a set-like list of player [p] and
    all the terrorites [p] owns. *)
let assoc_territories_helper (player : Player.t) =
  Player.get_territories player
  |> List.map (fun territory -> (Territory.get_name territory, player))

(** [assoc_territories p] will return a set-like list of all players and all
    the terrorites they owns.
    Example: [(terrority_name_v1, player_1);
    (terrority_name_v1, player_1)...] *)
let assoc_territories (player_list : player_list) =
  List.concat (List.map assoc_territories_helper player_list)

(** [print_label n t l] will return a string with ANSI styles applied to [n],
    [t], and [l] *)
let print_label territory_name territories label =
  (sprintf (Player.get_styles (List.assoc territory_name territories)) label)

(** [get_troop_count t n] gets the troop count for [n] in [t] *)
let get_troop_count territories territory_name =
  let player = List.assoc territory_name territories in
  let territory =
    Utility.get_territory_by_name territory_name (Player.get_territories player)
  in
  Territory.get_count territory

(** [print_troops_format n] formats [n] to three digits for the printed map *)
let print_troops_format num =
  match String.length (string_of_int num) with
  | 1 -> "00" ^ string_of_int num
  | 2 -> "0" ^ string_of_int num
  | _ -> string_of_int num

(** [print_troops t] will return [t] with bold style that includes the troop
    count for the territiory with this name *)
let print_troops (territory_name : string) territories =
  sprintf [Bold] ("%s") ((get_troop_count territories territory_name)
                         |> print_troops_format)

(** [print_map t] prints the game map with the information retrieved from
    territories [t] *)
let print_map territories =
  ANSITerminal.(print_string []
                  ("                                                          +-------------+
                                                          |             |
                                                          |             |
                                                          |  "^(print_label "Greenland" territories "Greenland")^"  |
                            +----------------+--------+---+             |                                                          +-------------------+
                            |                |        |   |    "^(print_troops "Greenland" territories)^"      |                                                          |     "^(print_label "Yakutsk" territories "Yakutsk")^"       |
                            |                |        |   |             |                              +-------------+-------------+       "^(print_troops "Yakutsk" territories)^"         +------------+
                            |                |        |   +-----+-------+                              |             |             |                   |            |
          +-----------------------------+    |        |         |           +---------------+          |             |   "^(print_label "Siberia" territories "Siberia")^"   +-------------------+  "^(print_label "Kamchatka" territories "Kamchatka")^" |
          |                             |    |        |         |  +--------+               +----------+   "^(print_label "Ural" territories "Ural")^"      |             |    "^(print_label "Irkutsk" territories "Irkutsk")^"        |            |
          |  "^(print_label "Northwest_Terr" territories "Northwest_Terr")^"             |    |        |         |  |        |"^(print_label "Scandinavia" territories "Scandinavia")^"    |          |             |     "^(print_troops "Siberia" territories)^"     |      "^(print_troops "Irkutsk" territories)^"          |    "^(print_troops "Kamchatka" territories)^"     |
+---------+             "^(print_troops "Northwest_Terr" territories)^"             |    |  +-----+---+    ++--+---+    |    "^(print_troops "Scandinavia" territories)^"        | "^(print_label "Ukraine" territories "Ukraine")^"  |    "^(print_troops "Ural" territories)^"      |             |                   |            |
|         |                             |    |  |         |    |"^(print_label "Iceland" territories "Iceland")^"|    +---+-----------+          |             |             +-------------------+            |
|  "^(print_label "Alaska" territories "Alaska")^" +--------------+--------------+----+--+         |    |  "^(print_troops "Iceland" territories)^"  |    |   |           |  "^(print_troops "Ukraine" territories)^"     |             |             |     "^(print_label "Mongolia" territories "Mongolia")^"      +-------+----+
|         |              |                      | "^(print_label "Quebec" territories "Quebec")^"  |    +----+--+    |   |           |          |             |             |        "^(print_troops "Mongolia" territories)^"        |       |
|   "^(print_troops "Alaska" territories)^"   |   "^(print_label "Alberta" territories "Alberta")^"    |     "^(print_label "Ontario" territories "Ontario")^"          |         |         |       |   | "^(print_label "N_Europe" territories "N_Europe")^"  |          |             +-------------+------+-------+----+       |
|         |              |                      |   "^(print_troops "Quebec" territories)^"   |         +-------+   |           +----+     |             |                    |       |            |
+---------+     "^(print_troops "Alberta" territories)^"      |       "^(print_troops "Ontario" territories)^"            |         |         |"^(print_label "Britain" territories "Britain")^"+-+-+   "^(print_troops "N_Europe" territories)^"     |    |     +-------------+                    |       |      +-----+---+
          |              |                      |         |         |  "^(print_troops "Britain" territories)^"  | | |           |    |     |             |      "^(print_label "China" territories "China")^"         |       |      |         |
          |              |                      |         |         +---+---+ | |           |    |     |  "^(print_label "Kazakhstan" territories "Kazakhstan")^" |                    |       |      |  "^(print_label "Japan" territories "Japan")^"  |
          +--------------+-----+----------------+------+--+             |     | +-----------+    |     |             |        "^(print_troops "China" territories)^"         |       |      |         |
          |                    |                       |                |     | |                |     |    "^(print_troops "Kazakhstan" territories)^"      |                    |       +------+   "^(print_troops "Japan" territories)^"   |
          |                    |        "^(print_label "Eastern_US" territories "Eastern_US")^"     |             +--+-----+-+   "^(print_label "S_Europe" territories "S_Europe")^"     |     |             |                    |              |         |
          |      "^(print_label "Western_US" territories "Western_US")^"    |                       |             |          |     "^(print_troops "S_Europe" territories)^"        |     |             |                    |              |         |
          |                    |          "^(print_troops "Eastern_US" territories)^"          |             | "^(print_label "W_Europe" territories "W_Europe")^" |                |     |             |                    |              |         |
          |       "^(print_troops "Western_US" territories)^"          |                       |             |   "^(print_troops "W_Europe" territories)^"    +---+----+-------+-----+---+---------+-----+----------+---+              +---------+
          |                    |                       |             |          |   |    |                 |               |          |
          +--------------------+----+------------------+             +--+-------+   |    |                 |               |  "^(print_label "Siam" territories "Siam")^"    |
          |                         |                                   |           |    |   "^(print_label "Middle_East" territories "Middle_East")^"   |    "^(print_label "India" territories "India")^"      |          |
          |       "^(print_label "Central_America" territories "Central_America")^"   |                                   | +---------+    |                 |               |   "^(print_troops "Siam" territories)^"    |
          |                         |                                   | |         |    |      "^(print_troops "Middle_East" territories)^"        |      "^(print_troops "India" territories)^"      |          |
          |         "^(print_troops "Central_America" territories)^"             |                                   | |         |    |                 |               |          |          +------------------+
          |                         |                                   | |         |    |                 |               +--+-------+-----+    |                  |
          |                         |                                   | |  +------+----+-------+         +---------------+  |             |    | "^(print_label "Papua_New_Guinea" territories "Papua_New_Guinea")^" |
          +-------+----------+------+                  +----------------+-+--+                   |         |                  | "^(print_label "Indonesia" territories "Indonesia")^"   |    |                  |
                  |          |                         |                     |      "^(print_label "Egypt" territories "Egypt")^"        |         |                  |    "^(print_troops "Indonesia" territories)^"      +----+      "^(print_troops "Papua_New_Guinea" territories)^"         |
                  | "^(print_label "Venezuela" territories "Venezuela")^"|                         |     "^(print_label "North_Africa" territories "North_Africa")^"    |                   |         |                  |             |    |                  |
                  |   "^(print_troops "Venezuela" territories)^"    +---------------+         |                     |      "^(print_troops "Egypt" territories)^"          |         |                  +-----+-------+    +--+-------+-------+
                  |          |               +---------+         "^(print_troops "North_Africa" territories)^"         +-------------------+         |                        |               |       |
                  +----------+               |         |                     |                   |         |                        |               |       |
                  |          |   "^(print_label "Brazil" territories "Brazil")^"      |         +------------+--------+----+              +---------+                        |               |       |
                  |  "^(print_label "Peru" territories "Peru")^"    |               |                      |             |              |                                  |               |       |
                  |          |      "^(print_troops "Brazil" territories)^"      |                      |             |              |                                  |               |       |
                  |   "^(print_troops "Peru" territories)^"    |               |                      |   "^(print_label "Congo" territories "Congo")^"     |   "^(print_label "E_Africa" territories "E_Africa")^"   |                                  |    +----------+--+----+--------+
                  |          |               |                      |             |              |                                  |    |             |             |
                  +----------+--+------------+                      |    "^(print_troops "Congo" territories)^"      |      "^(print_troops "E_Africa" territories)^"     |                                  +----+             |             |
                  |             |                                   |             |              |       +-------------+                 | "^(print_label "W_Australia" territories "W_Australia")^" | "^(print_label "E_Australia" territories "E_Australia")^" |
                  |             |                                   |             |              +-------+             |                 |             |             |
                  |  "^(print_label "Argentina" territories "Argentina")^"  |                                   |             |              |       |  "^(print_label "Madagascar" territories "Madagascar")^" |                 |    "^(print_troops "W_Australia" territories)^"      |    "^(print_troops "E_Australia" territories)^"      |
                  |             |                                   +-------------+--------------+       |             |                 |             |             |
                  |     "^(print_troops "Argentina" territories)^"     |                                   |         "^(print_label "South_Africa" territories "South_Africa")^"       |       |    "^(print_troops "Madagascar" territories)^"      |                 |             |             |
                  |             |                                   |            "^(print_troops "South_Africa" territories)^"             +-------+             |                 |             |             |
                  +-------------+                                   |                            |       +-------------+                 +-------------+-------------+
                                                                    +----------------------------+
"));;
