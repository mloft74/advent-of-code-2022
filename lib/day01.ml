let calories_per_elf_from_lines lines =
  let number_options = List.map int_of_string_opt lines in
  let calories_per_elf =
    List.fold_left (
      fun accumulator x ->
        match x with
        | None -> 0 :: accumulator
        | Some n ->
          match accumulator with
          | [] -> [n]
          | head :: tail -> (head + n) :: tail
    ) [] number_options
  in
  List.rev calories_per_elf

let part_1 calories_per_elf =
  List.fold_left (fun max x -> if x > max then x else max) 0 calories_per_elf

let part_2 calories_per_elf =
  let calories_per_elf = List.sort Int.compare calories_per_elf in
  let calories_per_elf = List.rev calories_per_elf in
  let top_3 = ListUtil.take 3 calories_per_elf in
  List.fold_left (+) 0 top_3

let run () = RunUtil.run_day 1 (
  fun lines ->
    let calories_per_elf = calories_per_elf_from_lines lines in
    Printf.printf "top calories: %d\n" (part_1 calories_per_elf);
    Printf.printf "combined top 3 calories: %d\n" (part_2 calories_per_elf);
    ()
)

(* #### TESTS #### *)

let lines = [
  "1000";
  "2000";
  "3000";
  "";
  "4000";
  "";
  "5000";
  "6000";
  "";
  "7000";
  "8000";
  "9000";
  "";
  "10000";
]

let calories_per_elf = calories_per_elf_from_lines lines

let%test _ =
  calories_per_elf = [6000; 4000; 11000; 24000; 10000]

(* part 1 *)
let%test _ =
  let res = part_1 calories_per_elf in
  res = 24000

(* part 2 *)
let%test _ =
  let res = part_2 calories_per_elf in
  res = 45000
