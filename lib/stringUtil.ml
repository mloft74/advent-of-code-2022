exception InvalidInputString of string

let split_in_2_on_char char input =
  let split = String.split_on_char char input in
  match split with
  | [a; b] -> (a, b)
  | _ -> raise (InvalidInputString input)

(*https://stackoverflow.com/a/8373836*)
let contains input search_value =
  let regexp = Str.regexp_string search_value in
  try ignore (Str.search_forward regexp input 0); true with
  | Not_found -> false

(* #### TESTS #### *)

let%test _ =
  let input = "12 13" in
  let res = split_in_2_on_char ' ' input in
  res = ("12", "13")

let%test _ =
  let input = "12" in
  let res = 
    try Some (split_in_2_on_char ' ' input) with
    | InvalidInputString _ -> None
  in
  res = None

let%test _ =
  let input = "12 13 14" in
  let res = 
    try Some (split_in_2_on_char ' ' input) with
    | InvalidInputString _ -> None
  in
  res = None

let%test _ =
  let input = "mary had a little lamb" in
  let search = "ry had a lit" in
  contains input search

let%test _ =
  let input = "mary had a little lamb" in
  let search = "ry [;] had a lit" in
  not (contains input search)
