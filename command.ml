type attack_phrase = { from_trr_name: string; to_trr_name: string}
type place_phrase = { count: int; trr_name: string}
type fortify_phrase = { count: int; from_trr_name: string; to_trr_name: string}

exception Malformed of string
exception Empty of string

type command =
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Next

let parse_attack (tokens : string list) : attack_phrase =
  try
    match tokens with
    | h1 :: h2 :: _ -> 
      {
        from_trr_name = h1;
        to_trr_name = h2;
      }
    | _ -> raise (Malformed "Malformed attack command")
  with _ -> raise (Malformed "Malformed attack command; please try again")

let parse_place (tokens : string list) : place_phrase =
  try 
    match tokens with
    | h1 :: h2 :: _ -> 
      {
        count = int_of_string h1;
        trr_name = h2
      }
    | _ -> raise (Malformed "Malformed place command")
  with _ -> raise (Malformed "Malformed place command; please try again")

let parse_fortify (tokens : string list) : fortify_phrase =
  try 
    match tokens with
    | h1 :: h2 :: h3 :: _ -> 
      {
        count = int_of_string h1;
        from_trr_name = h2;
        to_trr_name = h3;
      }
    | _ -> raise (Malformed "Malformed fortify command")
  with _ -> raise (Malformed "Malformed fortify command; please try again")

let parse str =
  let tokenized_str =
    str
    |> String.trim
    |> String.split_on_char ' '
    |> List.filter (fun token -> 
        if token = "" 
        then false 
        else true)
  in
  match tokenized_str with
  | [] -> raise (Empty "Empty command; please try again")
  | h :: t -> 
    begin
      match (String.lowercase_ascii h) with
      | "attack" -> Attack (parse_attack t)
      | "place" -> Place (parse_place t)
      | "fortify" -> Fortify (parse_fortify t)
      | "next" -> Next
      | _ -> raise (Malformed "Unrecognized command; please try again")
    end
