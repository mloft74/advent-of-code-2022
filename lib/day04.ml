let get_range_pairs_from_lines lines =
  List.map(
    fun line ->
      let (a, b) = StringUtil.split_in_2_on_char ',' line in
      let (a_low, a_high) = StringUtil.split_in_2_on_char '-' a in
      let (b_low, b_high) = StringUtil.split_in_2_on_char '-' b in
      (
        Range.create (int_of_string a_low) (int_of_string a_high),
        Range.create (int_of_string b_low) (int_of_string b_high)
      )
  ) lines

let part_1 range_pairs =
  List.fold_left (
    fun total (a, b) ->
      match Range.get_overlap_opt a b with
      | None -> total
      | Some r ->
        if r = a || r = b then
          total + 1
        else
          total
  ) 0 range_pairs

let part_2 range_pairs =
  List.fold_left (
    fun total (a, b) ->
      match Range.get_overlap_opt a b with
      | None -> total
      | Some _ -> total + 1
  ) 0 range_pairs

let run () = RunUtil.run_day 4 (
  fun lines ->
    let range_pairs = get_range_pairs_from_lines lines in
    Printf.printf "pairs where one contains the other: %d\n" (part_1 range_pairs);
    Printf.printf "pairs that overlap: %d\n" (part_2 range_pairs);
    ()
)

(* #### TESTS #### *)

let lines = [
  "2-4,6-8";
  "2-3,4-5";
  "5-7,7-9";
  "2-8,3-7";
  "6-6,4-6";
  "2-6,4-8";
]

(* ensure parsing works *)
let%test _ =
  let res = get_range_pairs_from_lines lines in
  res = [
    (Range.create 2 4, Range.create 6 8);
    (Range.create 2 3, Range.create 4 5);
    (Range.create 5 7, Range.create 7 9);
    (Range.create 2 8, Range.create 3 7);
    (Range.create 6 6, Range.create 4 6);
    (Range.create 2 6, Range.create 4 8);
  ]

let range_pairs = get_range_pairs_from_lines lines

(* test part 1 with sample data *)
let%test _ =
  let res = part_1 range_pairs in
  res = 2

(* test part 2 with sample data *)
let%test _ =
  let res = part_2 range_pairs in
  res = 4
