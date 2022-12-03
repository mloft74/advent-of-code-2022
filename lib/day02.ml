type shape = Rock | Paper | Scissors
let get_shape_value shape =
  match shape with
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
let get_shape_name shape =
  match shape with
  | Rock -> "Rock"
  | Paper -> "Paper"
  | Scissors -> "Scissors"

type outcome = Loss | Draw | Win
let get_outcome_value outcome =
  match outcome with
  | Loss -> 0
  | Draw -> 3
  | Win -> 6
let get_outcome_name outcome =
  match outcome with
  | Loss -> "Loss"
  | Draw -> "Draw"
  | Win -> "Win"

type round_choices = {
  opponent: shape;
  player: shape;
}

type round_choices_outcome = {
  round_choices: round_choices;
  outcome: outcome;
}

let get_round_choices_outcome_from_round_choices round_choices = {
  round_choices;
  outcome = match round_choices with
  | {  opponent = Paper; player = Rock; } | {  opponent = Scissors; player = Paper; } | {  opponent = Rock; player = Scissors; } -> Loss
  | {  opponent = Rock; player = Paper; } | {  opponent = Paper; player = Scissors; } | {  opponent = Scissors; player = Rock; } -> Win
  | _ -> Draw
  ;
}

let get_total_score_from_round_choices_outcomes =
  List.fold_left (fun acc round_outcome ->
    acc + (get_outcome_value round_outcome.outcome) + (get_shape_value round_outcome.round_choices.player)
  ) 0

let part_1 round_strings =
  let round_choices = List.map (fun (opponent, player) -> {
      opponent = (
        match opponent with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> assert false
      );
      player = (
        match player with
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | _ -> assert false
      );
    }
  ) round_strings in
  let round_choices_outcomes = List.map get_round_choices_outcome_from_round_choices round_choices in
  let total_score = get_total_score_from_round_choices_outcomes round_choices_outcomes in
  Printf.printf "part 1 total score: %d\n" total_score;
  ()

type round_plan = {
  opponent: shape;
  outcome: outcome;
}

let get_player_choice_from_round_plan round_plan =
  match round_plan with
  | { opponent = Rock; outcome = Draw } | { opponent = Scissors; outcome = Win } | { opponent = Paper; outcome = Loss } -> Rock
  | { opponent = Rock; outcome = Win } | { opponent = Scissors; outcome = Loss } | { opponent = Paper; outcome = Draw } -> Paper
  | _ -> Scissors

let get_round_choices_outcome_from_round_plan round_plan = {
  outcome = round_plan.outcome;
  round_choices = {
    opponent = round_plan.opponent;
    player = get_player_choice_from_round_plan round_plan;
  };
}

let part_2 round_strings =
  let round_plans = List.map (fun (opponent, outcome) -> {
      opponent = (
        match opponent with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> assert false
      );
      outcome = (
        match outcome with
        | "X" -> Loss
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> assert false
      );
    }
  ) round_strings in
  let round_choices_outcomes = List.map get_round_choices_outcome_from_round_plan round_plans in
  let total_score = get_total_score_from_round_choices_outcomes round_choices_outcomes in
  Printf.printf "part 2 total score: %d\n" total_score;
  ()

let run () = RunUtil.run_day 2 (fun lines ->
  let round_options = List.map (fun x ->
    let res =  String.split_on_char ' ' x in
    match res with
    | [a; b] -> Some (a, b)
    | _ -> None
  ) lines in
  let round_strings = List.filter_map (fun x -> x) round_options in
  part_1 round_strings;
  part_2 round_strings
)
