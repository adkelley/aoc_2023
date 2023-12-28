(* ./lib/util.ml *)

let read_whole_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let split_lines (input : string) : string list =
  input |> String.trim |> String.split_on_char '\n' |> List.map (String.trim)
