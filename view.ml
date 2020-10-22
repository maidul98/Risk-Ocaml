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
(* sprintf (Player.styles (List.assoc "Alaska" territories)) "Alaska" *)
let print_map (territories) =
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
|  "^sprintf (Player.styles (List.assoc "Alaska" territories)) "Alaska"^" +--------------+--------------+-------+         |    |       |    |   |           |          |             |             |     Mongolia      +-------+----+
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
\n"));;
