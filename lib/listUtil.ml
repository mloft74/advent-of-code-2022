type 'a separated_list = {
  head: 'a list;
  tail: 'a list
}

(** Returns a [separated_list] where [head] contains [n] items and [tail] contains the rest.

    If [n] is negative, [head] will be empty and [tail] will contain [list].

    If [n] is greater than the length of [list], [head] will contain the whole list and [tail] will be empty. *)
let separate n list =
  if n < 0 then
    { head = []; tail = list }
  else (
    let rec inner_split accumulator current_depth current_list =
      if current_depth = n then
        { head = accumulator; tail = current_list; }
      else 
        match current_list with
        | [] ->
          { head = accumulator; tail = [] }
        | head :: tail ->
          inner_split (head :: accumulator) (current_depth + 1) tail
    in
    let res = inner_split [] 0 list in
    { res with head = List.rev res.head }
  )

(** Calls [separate] and returns [head]. See also: [separate]. *)
let take n list =
  (separate n list).head

(** Calls [separate] and returns [tail]. See also: [separate]. *)
let skip n list =
  (separate n list).tail

(* #### TESTS #### *)

(* [separate] works when n > 0 && n < List.length list *)
let%test _ =
  let res = separate 2 [1; 2; 3; 4] in
  res.head = [1; 2] && res.tail = [3; 4]

(* [separate] works when n = List.length list *)
let%test _ =
  let res = separate 4 [1; 2; 3; 4] in
  res.head = [1; 2; 3; 4] && res.tail = []

(* [separate] works when n > List.length list *)
let%test _ =
  let res = separate 5 [1; 2; 3; 4] in
  res.head = [1; 2; 3; 4] && res.tail = []

(* [separate] works when n < 0 *)
let%test _ =
  let res = separate (-1) [1; 2; 3; 4] in
  res.head = [] && res.tail = [1; 2; 3; 4]

(* [separate] works when list is empty *)
let%test _ =
  let res = separate 1 [] in
  res.head = [] && res.tail = []

(* [take] returns [separated_list.head] *)
let%test _ =
  let list = [1; 2; 3; 4] in
  let num_to_take = 2 in
  let take_res = take num_to_take list in
  let separate_res = separate num_to_take list in
  take_res = separate_res.head

(* [skip] returns [separated_list.tail] *)
let%test _ =
  let list = [1; 2; 3; 4] in
  let num_to_skip = 2 in
  let take_res = skip num_to_skip list in
  let separate_res = separate num_to_skip list in
  take_res = separate_res.tail
