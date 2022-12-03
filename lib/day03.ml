module CharSet = Set.Make (Char)

exception InvalidCharArg of char
exception CharMatchNotFound
exception NotGroupOfThree
exception InvalidListSize

let value_of_char char =
  let lower_a = int_of_char 'a' in
  let lower_z = int_of_char 'z' in
  let upper_a = int_of_char 'A' in
  let upper_z = int_of_char 'Z' in
  let raw_char_value = int_of_char char in
  if raw_char_value >= lower_a && raw_char_value <= lower_z then
    raw_char_value - (lower_a - 1)
  else if raw_char_value >= upper_a && raw_char_value <= upper_z then
    raw_char_value - (upper_a - 1) + 26
  else
    raise (InvalidCharArg char)

let find_matching_char list =
  let char_set_from_string =
    String.fold_left (fun set char -> CharSet.add char set) CharSet.empty
  in
  match list with
  | first :: second :: tail ->
      let second_set = char_set_from_string second in
      let tail_sets = List.map char_set_from_string tail in
      let set = List.fold_left CharSet.inter second_set tail_sets in
      let start_value = '0' in
      let matched_char =
        String.fold_left (
          fun matched char ->
            if CharSet.mem char set then
              char
            else
              matched
        ) start_value first
      in
      if matched_char = start_value then
        raise CharMatchNotFound
      else
        matched_char
  | _ -> raise InvalidListSize

let first_part lines =
  let matched_chars =
    List.map (
      fun line ->
        let half_length = String.length line / 2 in
        let first_half = String.sub line 0 half_length in
        let last_half = String.sub line half_length half_length in
        find_matching_char [first_half; last_half]
    ) lines
  in
  let sum =
    List.fold_left (fun sum char -> sum + value_of_char char) 0 matched_chars
  in
  Printf.printf "first part sum: %d\n" sum;
  ()

let second_part lines =
  let rec group_in_threes accumulator lines =
    match ListUtil.separate 3 lines with
    | { head = [_; _; _] as head; tail = [] } -> head :: accumulator
    | { head; tail } -> group_in_threes (head :: accumulator) tail
  in
  let groups = group_in_threes [] lines in
  let matched_chars = List.map find_matching_char groups in
  let sum =
    List.fold_left (fun sum char -> sum + value_of_char char) 0 matched_chars
  in
  Printf.printf "second part sum: %d\n" sum;
  ()

let run () = RunUtil.run_day 03 (
  fun lines ->
    (* let lines = ["vJrwpWtwJgWrhcsFMMfFFhFp"; "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"; "PmmdzqPrVvPwwTWBwg"; "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"; "ttgJtRGJQctTZtZT"; "CrZsJsPPZsGzwwsLwLmpwMDw"] in *)
    first_part lines;
    second_part lines
)
