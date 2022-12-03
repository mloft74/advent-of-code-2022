let take n list =
  let rec inner_take accumulator current_depth current_list =
    if current_depth = n then
      accumulator
    else
      match current_list with
      | [] -> current_list
      | head :: tail -> inner_take (head :: accumulator) (current_depth + 1) tail
    in

  List.rev (inner_take [] 0 list)
