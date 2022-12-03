let run_day day_num func =
  Printf.printf "---- Day %02d: begin ----\n" day_num;
  func (Io.read_lines (Printf.sprintf "input/day%02d/input.txt" day_num));
  Printf.printf "---- Day %02d: cease ----\n\n" day_num;
  ()
