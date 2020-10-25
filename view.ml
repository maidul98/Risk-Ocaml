open ANSITerminal
type player_list = Player.t list

type territory_assoc = (Territory.territory_name * Player.t) list

(** Given a player [player], this will return a set-like list of a player
    and all the terrorites they owns.
    Example: [(terrority_name_v1, player_name);
    (terrority_name_v2, player_name)...] *)
let helper1 (player: Player.t) =
  Player.territories player |>
  List.map (fun territory -> (Territory.name territory, player))

let assoc_territories (player_list : player_list ) =
  List.concat (List.map helper1 player_list)

let print_label territory_name territories label = 
  (sprintf (Player.styles (List.assoc territory_name territories)) label)



(* sprintf (Player.styles (List.assoc "Alaska" territories)) "Alaska" *)
let print_map (territories) =
  ANSITerminal.(print_string []                  ("\n\n                                                          +-------------+
                                                          |             |
                                                          |             |
                                                          |  "^(print_label "Greenland" territories "Greenland")^"  |
                                                      +---+             |                                                          +-------------------+
                                                      |   |             |                                                          |     "^(print_label "Yakutsk" territories "Yakutsk")^"       |
                                                      |   |             |                              +-------------+-------------+                   +------------+
                                                      |   +-----+-------+                              |             |             |                   |            |
          +-----------------------------+             |         |           +---------------+          |             |   Siberia   +-------------------+  Kamchatka |
          |                             |             |         |  +--------+               +----------+   "^(print_label "Ural" territories "Ural")^"      |             |    "^(print_label "Irkutsk" territories "Irkutsk")^"        |            |
          |  "^(print_label "Northwest Territory" territories "Northwest Terr.")^"            |             |         |  |        |Scandinavia    |          |             |             |                   |            |
+---------+                             |       +-----+---+    ++--+---+    |               | Ukraine  |             |             |                   |            |
|         |                             |       |         |    |"^(print_label "Iceland" territories "Iceland")^"|    +---+-----------+          |             |             +-------------------+            |
|  "^(print_label "Alaska" territories "Alaska")^" +--------------+--------------+-------+         |    |       |    |   |           |          |             |             |     "^(print_label "Mongolia" territories "Mongolia")^"      +-------+----+
|         |              |                      | Quebec  |    +----+--+    |   |           |          |             |             |                   |       |
|         |   "^(print_label "Alberta" territories "Alberta")^"    |     Ontario          |         |         |       |   | N Europe  |          |             +-------------+------+-------+----+       |
|         |              |                      |         |         +-------+   |           +----+     |             |                    |       |            |
+---------+              |                      |         |         |Britain|   |           |    |     +-------------+                    |       |      +-----+---+
          |              |                      |         |         |       |   |           |    |     |             |      "^(print_label "China" territories "China")^"         |       |      |         |
          |              |                      |         |         +---+---+   |           |    |     |  Kazakhstan |                    |       |      |  "^(print_label "Japan" territories "Japan")^"  |
          +--------------+-----+----------------+------+--+             |       +-----------+    |     |             |                    |       |      |         |
          |                    |                       |                |       |                |     |             |                    |       +------+         |
          |                    |        Eastern US     |             +--+-------+   S Europe     |     |             |                    |              |         |
          |      "^(print_label "Western U.S." territories "Western US")^"    |                       |             |          |                |     |             |                    |              |         |
          |                    |                       |             | W Europe |                |     |             |                    |              |         |
          |                    |                       |             |          +------------+---+-----+---+---------+-----+----------+---+              +---------+
          |                    |                       |             |          |            |             |               |          |
          +--------------------+----+------------------+             +---+------+            |             |               |  Siam    |
          |                         |                                    |                   |   Middle    |    India      |          |
          |       "^(print_label "Central America" territories "Central America")^"   |                                    |                   |   East      |               |          |
          |                         |                                    |                   |             |               |          |
          |                         |                                    |                   |             |               |          |          +-------------+
          |                         |                                    |                   |             |               +--+-------+-----+    |             |
          |                         |                                    |   +---------------+---+         +---------------+  |             |    |  Papua New  |
          +-------+----------+------+                  +-----------------+---+                   |         |                  | Indonesia   |    |  Guinea     |
                  |          |                         |                     |      Egypt        |         |                  |             +----+             |
                  | "^(print_label "Venezuela" territories "Venezuela")^"|                         |     North Africa    |                   |         |                  |             |    |             |
                  |          +---------------+         |                     |                   |         |                  +-----+-------+    +--+-------+--+
                  |          |               +---------+                     +-------------------+         |                        |               |       |
                  +----------+               |         |                     |                   |         |                        |               |       |
                  |          |   Brazil      |         +------------+--------+----+              +---------+                        |               |       |
                  |  "^(print_label "Peru" territories "Peru")^"    |               |                      |             |              |                                  |               |       |
                  |          |               |                      |             |    East      |                                  |               |       |
                  |          |               |                      |   Congo     |    Africa    |                                  |       +-------+---+---+--------+
                  |          |               |                      |             |              |                                  |       |           |            |
                  +----------+--+------------+                      |             |              |                                  +-------+ Western   |  Eastern   |
                  |             |                                   |             |              |       +-------------+                    | Australia |  Australia |
                  |             |                                   |             |              +-------+             |                    |           |            |
                  |  "^(print_label "Argentina" territories "Argentina")^"  |                                   |             |              |       |  Madagascar |                    |           |            |
                  |             |                                   +-------------+--------------+       |             |                    |           |            |
                  |             |                                   |         South Africa       |       |             |                    |           |            |
                  |             |                                   |                            +-------+             |                    |           |            |
                  +-------------+                                   |                            |       +-------------+                    +-----------+------------+
                                                                    +----------------------------+
\n"));;

(* let print_map (territories) =
   ANSITerminal.(print_string []                  ("\n\n                                                          +-------------+
                                                          |             |
                                                          |             |
                                                          |  Greenland  |
                                                      +---+             |                                                          +-------------------+
                                                      |   |             |                                                          |     Yakutsk       |
                                                      |   |             |                              +-------------+-------------+                   +------------+
                                                      |   +-----+-------+                              |             |             |                   |            |
          +-----------------------------+             |         |           +---------------+          |             |   Siberia   +-------------------+  Kamchatka |
          |                             |             |         |  +--------+               +----------+   Urai      |             |    Irkutsk        |            |
          |  Northwest Terr.            |             |         |  |        |Scandinavia    |          |             |             |                   |            |
   +---------+                             |       +-----+---+    ++--+---+    |               | Ukraine  |             |             |                   |            |
   |         |                             |       |         |    |Iceland|    +---+-----------+          |             |             +-------------------+            |
   |  "^(sprintf (Player.styles (List.assoc "Alaska" territories)) "Alaska")^" +--------------+--------------+-------+         |    |       |    |   |           |          |             |             |     Mongolia      +-------+----+
   |         |              |                      | Quebec  |    +----+--+    |   |           |          |             |             |                   |       |
   |         |   Alberta    |     Ontario          |         |         |       |   | N Europe  |          |             +-------------+------+-------+----+       |
   |         |              |                      |         |         +-------+   |           +----+     |             |                    |       |            |
   +---------+              |                      |         |         |Britain|   |           |    |     +-------------+                    |       |      +-----+---+
          |              |                      |         |         |       |   |           |    |     |             |      China         |       |      |         |
          |              |                      |         |         +---+---+   |           |    |     |  Kazakhstan |                    |       |      |  Japan  |
          +--------------+-----+----------------+------+--+             |       +-----------+    |     |             |                    |       |      |         |
          |                    |                       |                |       |                |     |             |                    |       +------+         |
          |                    |        Eastern US     |             +--+-------+   S Europe     |     |             |                    |              |         |
          |      Western US    |                       |             |          |                |     |             |                    |              |         |
          |                    |                       |             | W Europe |                |     |             |                    |              |         |
          |                    |                       |             |          +------------+---+-----+---+---------+-----+----------+---+              +---------+
          |                    |                       |             |          |            |             |               |          |
          +--------------------+----+------------------+             +---+------+            |             |               |  Siam    |
          |                         |                                    |                   |   Middle    |    India      |          |
          |       Central America   |                                    |                   |   East      |               |          |
          |                         |                                    |                   |             |               |          |
          |                         |                                    |                   |             |               |          |          +-------------+
          |                         |                                    |                   |             |               +--+-------+-----+    |             |
          |                         |                                    |   +---------------+---+         +---------------+  |             |    |  Papua New  |
          +-------+----------+------+                  +-----------------+---+                   |         |                  | Indonesia   |    |  Guinea     |
                  |          |                         |                     |      Egypt        |         |                  |             +----+             |
                  | Venezuela|                         |     North Africa    |                   |         |                  |             |    |             |
                  |          +---------------+         |                     |                   |         |                  +-----+-------+    +--+-------+--+
                  |          |               +---------+                     +-------------------+         |                        |               |       |
                  +----------+               |         |                     |                   |         |                        |               |       |
                  |          |   Brazil      |         +------------+--------+----+              +---------+                        |               |       |
                  |  Peru    |               |                      |             |              |                                  |               |       |
                  |          |               |                      |             |    East      |                                  |               |       |
                  |          |               |                      |   Congo     |    Africa    |                                  |       +-------+---+---+--------+
                  |          |               |                      |             |              |                                  |       |           |            |
                  +----------+--+------------+                      |             |              |                                  +-------+ Western   |  Eastern   |
                  |             |                                   |             |              |       +-------------+                    | Australia |  Australia |
                  |             |                                   |             |              +-------+             |                    |           |            |
                  |  Argentina  |                                   |             |              |       |  Madagascar |                    |           |            |
                  |             |                                   +-------------+--------------+       |             |                    |           |            |
                  |             |                                   |         South Africa       |       |             |                    |           |            |
                  |             |                                   |                            +-------+             |                    |           |            |
                  +-------------+                                   |                            |       +-------------+                    +-----------+------------+
                                                                    +----------------------------+
   \n"));; *)