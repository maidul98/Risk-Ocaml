type attack_phrase =
  {
    from_trr_name: string;
    to_trr_name: string
  }
type place_phrase =
  {
    count: int;
    trr_name: string
  }
type fortify_phrase =
  {
    count: int;
    from_trr_name: string;
    to_trr_name: string
  }

exception Malformed of string
exception Empty of string
exception Negative_int of string

type command =
  | Attack of attack_phrase
  | Place of place_phrase
  | Fortify of fortify_phrase
  | Next
  | Quit

(** [parse_attack t] converts [t] to an [attack_phrase] *)
let parse_attack (tokens : string list) : attack_phrase =
  try
    match tokens with
    | [] -> raise (Empty "Empty attack command")
    | h1 :: h2 :: _ ->
      {
        from_trr_name = h1;
        to_trr_name = h2;
      }
    | _ -> raise (Malformed "Malformed attack command")
  with
  | Negative_int m -> raise (Negative_int m)
  | Malformed m -> raise (Malformed m)
  | Empty m -> raise (Empty m)
  | _ -> raise (Malformed "Malformed place command; please try again")

(** [parse_place t] converts [t] to a [place_phrase] *)
let parse_place (tokens : string list) : place_phrase =
  try
    match tokens with
    | [] -> raise (Empty "Empty place command")
    | h1 :: h2 :: _ -> if int_of_string h1 < 0
      then raise (Negative_int "Cannot place negative troops") else
        {
          count = int_of_string h1;
          trr_name = h2
        }
    | _ -> raise (Malformed "Malformed place command")
  with
  | Negative_int m -> raise (Negative_int m)
  | Malformed m -> raise (Malformed m)
  | Empty m -> raise (Empty m)
  | _ -> raise (Malformed "Malformed place command; please try again")

(** [parse_fortify t] converts [t] to a [fortify_phrase] *)
let parse_fortify (tokens : string list) : fortify_phrase =
  try
    match tokens with
    | [] -> raise (Empty "Empty fortify command")
    | h1 :: h2 :: h3 :: _ -> if int_of_string h1 < 0
      then raise (Negative_int "Cannot move negative troops") else
        {
          count = int_of_string h1;
          from_trr_name = h2;
          to_trr_name = h3;
        }
    | _ -> raise (Malformed "Malformed fortify command")
  with
  | Negative_int m -> raise (Negative_int m)
  | Malformed m -> raise (Malformed m)
  | Empty m -> raise (Empty m)
  | _ -> raise (Malformed "Malformed fortify command; please try again")

(** [tokenized_str s] converts [s] to a string list *)
let tokenized_str str =
  str
  |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun token -> if token = "" then false else true)

(** [parse s] converts [s] to a [command] *)
let parse str =
  let str = tokenized_str str in
  match str with
  | [] -> raise (Empty "Empty command; please try again")
  | h :: t ->
    begin
      match (String.lowercase_ascii h) with
      | "attack" -> Attack (parse_attack t)
      | "place" -> Place (parse_place t)
      | "fortify" -> Fortify (parse_fortify t)
      | "next" -> Next
      | "quit" -> Quit
      | _ -> raise (Malformed "Unrecognized command; please try again")
    end
