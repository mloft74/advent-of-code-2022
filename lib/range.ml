(** A module for creating and working with discreet inclusive ranges. *)

type range = {
  low: int;
  high: int;
}

module RangeSet = Set.Make (Int)
exception RangeDecreases

let create_opt low high =
  if high < low then
    None
  else
    Some { low; high }

let create low high =
  match create_opt low high with
  | None -> raise RangeDecreases
  | Some r -> r

let low range = range.low

let high range = range.high

let length range = range.high - range.low + 1

let int_list_of_range range =
  List.init (length range) (fun i -> range.low + i)

let get_overlap_opt a b =
  let a_set = RangeSet.of_list (int_list_of_range a) in
  let b_set = RangeSet.of_list (int_list_of_range b) in
  let intersection = RangeSet.inter a_set b_set in
  let inter_min_opt = RangeSet.min_elt_opt intersection in
  let inter_max_opt = RangeSet.max_elt_opt intersection in
  match inter_min_opt with
  | None -> None
  | Some low ->
    match inter_max_opt with
    | None -> None
    | Some high -> Some (create low high)

(* #### TESTS #### *)

(* testing length *)
let%test _ =
  let range = create 2 12 in
  let res = length range in
  res = 11

(* ranges don't overlap *)
let%test _ =
  let range_a = create 7 13 in
  let range_b = create 14 17 in
  let res = get_overlap_opt range_a range_b in
  res = None

(* ranges do overlap *)
let%test _ =
  let range_a = create 7 13 in
  let range_b = create 10 17 in
  let res = get_overlap_opt range_a range_b in
  res = Some (create (low range_b) (high range_a))

(* one range contains the other *)
let%test _ =
  let contained_range = create 5 10 in
  let containing_range = create 0 20 in
  let res = get_overlap_opt contained_range containing_range in
  res = Some contained_range

(* arguments are commutative for no overlap*)
let%test _ =
  let range_a = create 400 420 in
  let range_b = create 690 700 in
  let first = get_overlap_opt range_a range_b in
  let second = get_overlap_opt range_a range_b in
  first = second

(* arguments are commutative for overlap *)
let%test _ =
  let range_a = create 5 14 in
  let range_b = create 8 23 in
  let first = get_overlap_opt range_a range_b in
  let second = get_overlap_opt range_b range_a in
  first = second

(* arguments are commutative for containment *)
let%test _ =
  let contained_range = create 50 70 in
  let containing_range = create 20 100 in
  let first = get_overlap_opt contained_range containing_range in
  let second = get_overlap_opt containing_range contained_range in
  first = second
