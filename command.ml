type attack_phrase = { from_trr_name: string; to_trr_name: string }
type place_phrase = { count: int; trr_name: string }
type fortify_phrase = { count: int; from_trr_name: string; to_trr_name: string }

exception Malformed of string
exception Empty

type command =
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Skip

let parse_attack (tokens : string list) : attack_phrase =
  match tokens with
  | h1 :: h2 :: _ -> {
      from_trr_name = h1;
      to_trr_name = h2;
    }
  | _ -> raise (Malformed "Malformed attack command")

let parse_place (tokens : string list) : place_phrase =
  match tokens with
  | h1 :: h2 :: _ -> {
      count = int_of_string h1;
      trr_name = h2
    }
  | _ -> raise (Malformed "Malformed place command")

let parse_fortify (tokens : string list) : fortify_phrase =
  match tokens with
  | h1 :: h2 :: h3 :: _ -> {
      count = int_of_string h1;
      from_trr_name = h2;
      to_trr_name = h3;
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
      | "skip" -> Skip
      | _ -> raise (Malformed "Unrecognized command")
    end
