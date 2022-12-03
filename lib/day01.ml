let run () = RunUtil.run_day 1 (fun lines ->
  let number_options = List.map int_of_string_opt lines in

  let calories_per_elf = List.fold_left (
    fun accumulator x ->
      match x with
      | None -> 0 :: accumulator
      | Some n ->
        match accumulator with
        | [] -> [n;]
        | head :: tail -> (head + n) :: tail
  ) [] number_options in

  let calories_per_elf = List.sort (
    fun a b ->
      if a = b then
        0
      else if a > b then
        -1
      else
        1
  ) calories_per_elf in

  let top_1 = ListUtil.take 1 calories_per_elf in
  let total_from_top_1 = List.fold_left (fun acc x -> acc + x) 0 top_1 in
  Printf.printf "top calories: %d\n" total_from_top_1;

  let top_3 = ListUtil.take 3 calories_per_elf in
  let total_from_top_3 = List.fold_left (fun acc x -> acc + x) 0 top_3 in
  Printf.printf "top 3 calories combined: %d\n" total_from_top_3;

  ()
)
