type attack_phrase = { from_country: string; to_country: string }
type place_phrase = { count: int; country: string }
type fortify_phrase = { count: int; from_country: string; to_country: string }

exception Malformed of string
exception Empty

type command =
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Quit
  | Skip

let parse_attack (tokens : string list) : attack_phrase =
  match tokens with
  | h1 :: h2 :: _ -> {
      from_country = h1;
      to_country = h2;
    }
  | _ -> raise (Malformed "Malformed attack command")

let parse_place (tokens : string list) : place_phrase =
  match tokens with
  | h1 :: h2 :: _ -> {
      count = int_of_string h1;
      country = h2
    }
  | _ -> raise (Malformed "Malformed place command")

let parse_fortify (tokens : string list) : fortify_phrase =
  match tokens with
  | h1 :: h2 :: h3 :: _ -> {
      count = int_of_string h1;
      from_country = h2;
      to_country = h3;
    }
  | _ -> raise (Malformed "Malformed fortify command")

let parse str =
  let tokenized_str =
    str
    |> String.trim
    |> String.split_on_char ' '
    |> List.filter (fun token -> if token = "" then false else true)
  in
  match tokenized_str with
  | [] -> raise Empty
  | h :: t -> begin
      match h with
      | "attack" -> Attack (parse_attack t)
      | "place" -> Place (parse_place t)
      | "fortify" -> Fortify (parse_fortify t)
      | "quit" -> Quit
      | "skip" -> Skip
      | _ -> raise (Malformed "Unrecognized command")
    end
