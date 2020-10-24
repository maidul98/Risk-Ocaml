type object_phrase = string list

exception Malformed

type command = 
  | Action of object_phrase
  | Quit

let parse str =
  let tokenized_str =
    str
    |> String.trim
    |> String.split_on_char ' ' 
    |> List.filter (fun token -> if token = "" then false else true)    
  in match tokenized_str with
  | [] -> None
  | h :: t -> begin
      match h with
      | "quit" -> Some Quit
      | "end" -> Some (Action t)
      | _ -> raise Malformed
    end
