(*https://stackoverflow.com/a/23456034*)

let read_lines file_name =
  let in_channel = open_in file_name in
  let try_read () =
    try Some (input_line in_channel) with End_of_file -> None in
  let rec loop accumulator =
    match try_read () with
    | Some s -> loop (s :: accumulator)
    | None ->
      close_in in_channel;
      List.rev accumulator in
  loop []
