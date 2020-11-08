type attack_detail = { from_country: string; to_country: string }
type place_detail = { count: int; country: string }
type fortify_detail = { count: int; from_country: string; to_country: string }

exception Malformed of string
exception Empty

type command = 
  | Attack of attack_detail
  | Place of place_detail
  | Fortify of fortify_detail
  | Quit
  | Skip

let parse_attack = function
  | h1 :: h2 :: _ -> {
      from_country = h1;
      to_country = h2;
    }
  | _ -> raise (Malformed "Malformed attack command")

let parse_place = function
  | h1 :: h2 :: _ -> {
      count = int_of_string h1;
      country = h2
    }
  | _ -> raise (Malformed "Malformed place command")

let parse_fortify = function
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
  in match tokenized_str with
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
