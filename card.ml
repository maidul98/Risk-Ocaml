type card_name = string

type card_locs = Territory.t list

type t = {
  name: card_name;
  territories: card_locs;
}

let init name = {
  name = name;
  territories = [];
}

let name card = card.name

let valid_locs card = card.territories

let add_territory card territory_add = {
  card with territories = territory_add :: card.territories
}
