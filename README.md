Core Vision: We aim to develop a multiplayer, local implementation of the strategy board game Risk.

Game Context: In Risk, the ultimate goal of the game is to conquer all the territories of the world. At the onset, players are randomly, uniformly distributed territories of the world and given troops to place onto the territories. As rounds pass, players are expected to conquer other players’ territories by probabilistically rolling dice and strategically moving troops through connected territories. All the while, players receive more troops and special cards redeemable for even more troops at the start and end of every round. The player who conquers all other players’ territories wins the game!

Description: We intend to reimplement a multiplayer and intelligent form of Risk using OCaml. We will implement the underlying game features and rules abstracted as data structures and conditions; here, we will represent territories and territory adjacencies as nodes and edges respectively. As part of the game features, we will implement everything from initialization features like territory assignment to gameplay features and conditions including player rounds, probabilistic conquest, troop-movement through connected territories and more. Finally, if our group has time, we will implement an intelligent system to play against users.

Key Features: In this section, we list some core features that players may expect from the finished product:

- Multiplayer & Single-Player Gameplay: Players may opt to play games locally with others or play with intelligent agents/bots (if project timeline allows).

- Player Rounds: Gameplay is split into rounds wherein each round involves a player receiving troops and cards as well as exercising his/her moves.

- Territory Conquest: At the start of each round, players may probabilistically attack other territories given they are adjacent to his/her own. If victorious, players acquire ownership over target territories in conjunction and retain ownership over their previous territories; otherwise, ownership rights remain unchanged.

- Card Redemption: At the start of each round, given valid combinations of cards, players may redeem combinations to gain additional troops that particular round.

- Troop Movement: At the end of each round, players may move troops through their owned territories as long as they are connected.
