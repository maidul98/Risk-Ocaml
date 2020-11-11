open ANSITerminal
type player_list = Player.t list

type territory_assoc = (Territory.territory_name * Player.t) list

(** Given a player [player], this will return a set-like list of a player
    and all the terrorites they owns.
    Example: [(terrority_name_v1, player_name);
    (terrority_name_v2, player_name)...] *)
let helper1 (player: Player.t) =
  Player.get_territories player |>
  List.map (fun territory -> (Territory.get_name territory, player))

let assoc_territories (player_list : player_list ) =
  List.concat (List.map helper1 player_list)

let print_label territory_name territories label = 
  (sprintf (Player.get_styles (List.assoc territory_name territories)) label)



(* sprintf (Player.styles (List.assoc "Alaska" territories)) "Alaska" *)
let print_map (territories) =
  ANSITerminal.(print_string []                  
                  ("                                                          +-------------+
                                                          |             |
                                                          |             |
                                                          |  "^(print_label "Greenland" territories "Greenland")^"  |
                                                      +---+             |                                                          +-------------------+
                                                      |   |    234      |                                                          |     "^(print_label "Yakutsk" territories "Yakutsk")^"       |
                                                      |   |             |                              +-------------+-------------+       234         +------------+
                                                      |   +-----+-------+                              |             |             |                   |            |
          +-----------------------------+             |         |           +---------------+          |             |   "^(print_label "Siberia" territories "Siberia")^"   +-------------------+  "^(print_label "Kamchatka" territories "Kamchatka")^" |
          |                             |             |         |  +--------+               +----------+   "^(print_label "Ural" territories "Ural")^"      |             |    "^(print_label "Irkutsk" territories "Irkutsk")^"        |            |
          |  "^(print_label "Northwest_Terr" territories "Northwest_Terr")^"             |             |         |  |        |"^(print_label "Scandinavia" territories "Scandinavia")^"    |          |             |     234     |      344          |    234     |
+---------+             234             |       +-----+---+    ++--+---+    |    234        | "^(print_label "Ukraine" territories "Ukraine")^"  |    234      |             |                   |            |
|         |                             |       |         |    |"^(print_label "Iceland" territories "Iceland")^"|    +---+-----------+          |             |             +-------------------+            |
|  "^(print_label "Alaska" territories "Alaska")^" +--------------+--------------+-------+         |    |  234  |    |   |           |  234     |             |             |     "^(print_label "Mongolia" territories "Mongolia")^"      +-------+----+
|         |              |                      | "^(print_label "Quebec" territories "Quebec")^"  |    +----+--+    |   |           |          |             |             |        234        |       |
|   234   |   "^(print_label "Alberta" territories "Alberta")^"    |     "^(print_label "Ontario" territories "Ontario")^"          |         |         |       |   | "^(print_label "N_Europe" territories "N_Europe")^"  |          |             +-------------+------+-------+----+       |
|         |              |                      |   234   |         +-------+   |           +----+     |             |                    |       |            |
+---------+     234      |       234            |         |         |"^(print_label "Britain" territories "Britain")^"|   |   234     |    |     +-------------+                    |       |      +-----+---+
          |              |                      |         |         |  234  |   |           |    |     |             |      "^(print_label "China" territories "China")^"         |       |      |         |
          |              |                      |         |         +---+---+   |           |    |     |  "^(print_label "Kazakhstan" territories "Kazakhstan")^" |                    |       |      |  "^(print_label "Japan" territories "Japan")^"  |
          +--------------+-----+----------------+------+--+             |       +-----------+    |     |             |        234         |       |      |         |
          |                    |                       |                |       |                |     |    234      |                    |       +------+   234   |
          |                    |        "^(print_label "Eastern_US" territories "Eastern_US")^"     |             +--+-------+   "^(print_label "S_Europe" territories "S_Europe")^"     |     |             |                    |              |         |
          |      "^(print_label "Western_US" territories "Western_US")^"    |                       |             |          |     234        |     |             |                    |              |         |
          |                    |          234          |             | "^(print_label "W_Europe" territories "W_Europe")^" |                |     |             |                    |              |         |
          |       234          |                       |             |   234    +--------+-------+-----+---+---------+-----+----------+---+              +---------+
          |                    |                       |             |          |        |                 |               |          |
          +--------------------+----+------------------+             +---+------+        |                 |               |  "^(print_label "Siam" territories "Siam")^"    |
          |                         |                                    |               |   "^(print_label "Middle_East" territories "Middle_East")^"   |    "^(print_label "India" territories "India")^"      |          |
          |       "^(print_label "Central_America" territories "Central_America")^"   |                                    |               |                 |               |   234    |
          |                         |                                    |               |      234        |      234      |          |
          |         234             |                                    |               |                 |               |          |          +------------------+
          |                         |                                    |               |                 |               +--+-------+-----+    |                  |
          |                         |                                    |   +-----------+-------+         +---------------+  |             |    | "^(print_label "Papua_New_Guinea" territories "Papua_New_Guinea")^" |
          +-------+----------+------+                  +-----------------+---+                   |         |                  | "^(print_label "Indonesia" territories "Indonesia")^"   |    |                  |
                  |          |                         |                     |      "^(print_label "Egypt" territories "Egypt")^"        |         |                  |    234      +----+      234         |
                  | "^(print_label "Venezuela" territories "Venezuela")^"|                         |     "^(print_label "North_Africa" territories "North_Africa")^"    |                   |         |                  |             |    |                  |
                  |   234    +---------------+         |                     |      234          |         |                  +-----+-------+    +--+-------+-------+
                  |          |               +---------+         234         +-------------------+         |                        |               |       |
                  +----------+               |         |                     |                   |         |                        |               |       |
                  |          |   "^(print_label "Brazil" territories "Brazil")^"      |         +------------+--------+----+              +---------+                        |               |       |
                  |  "^(print_label "Peru" territories "Peru")^"    |               |                      |             |              |                                  |               |       |
                  |          |      234      |                      |             |              |                                  |               |       |
                  |   234    |               |                      |   "^(print_label "Congo" territories "Congo")^"     |   "^(print_label "E_Africa" territories "E_Africa")^"   |                                  |    +----------+--+----+--------+
                  |          |               |                      |             |              |                                  |    |             |             |
                  +----------+--+------------+                      |    234      |      234     |                                  +----+             |             |
                  |             |                                   |             |              |       +-------------+                 | "^(print_label "W_Australia" territories "W_Australia")^" | "^(print_label "E_Australia" territories "E_Australia")^" |
                  |             |                                   |             |              +-------+             |                 |             |             |
                  |  "^(print_label "Argentina" territories "Argentina")^"  |                                   |             |              |       |  "^(print_label "Madagascar" territories "Madagascar")^" |                 |    234      |    234      |
                  |             |                                   +-------------+--------------+       |             |                 |             |             |
                  |     234     |                                   |         "^(print_label "South_Africa" territories "South_Africa")^"       |       |    234      |                 |             |             |
                  |             |                                   |            234             +-------+             |                 |             |             |
                  +-------------+                                   |                            |       +-------------+                 +-------------+-------------+
                                                                    +----------------------------+
"));;