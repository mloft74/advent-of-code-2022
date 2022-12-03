type 'a separated = {
  head: 'a list;
  tail: 'a list
}

let separate n list =
  let rec inner_split accumulator current_depth current_list =
    if current_depth = n then
      { head = accumulator; tail = current_list; }
    else
      match current_list with
      | [] -> { head = current_list; tail = [] }
      | head :: tail -> inner_split (head :: accumulator) (current_depth + 1) tail
    in

  let res = inner_split [] 0 list in
  { res with head = List.rev res.head }

let take n list =
  (separate n list).head

let skip n list =
  (separate n list).tail
