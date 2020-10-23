open Yojson.Basic.Util

type card_name = string

type card_locs = Territory.t list

type t = {
  card_name: card_name;
  territories: card_locs;
}

let init name = {
  card_name = name;
  territories = [];
}

let name card = card.card_name

let valid_locs card = card.territories
