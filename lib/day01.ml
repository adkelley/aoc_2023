(* lib/day01.ml *)

let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let sum : int list -> int = List.fold_left ( + ) 0

let lines (input : string) : string list =
  input |> String.trim |> String.split_on_char '\n'

let is_digit (c : char) : bool = '0' <= c && c <= '9'
let to_digit (c : char) : int = Char.code c - Char.code '0'

let rec find_digit (line : string) (f : int -> int) (i : int) : int =
  if is_digit line.[i] then to_digit line.[i] else find_digit line f (f i)

let decode (line : string) : int =
  let first_digit = find_digit line (fun x -> x + 1) 0 in
  let last_digit = find_digit line (fun x -> x - 1) (String.length line - 1) in
  first_digit * 10 + last_digit

(*
1. Split into lines
2. Find the first and last digit of each number
3. sum all numbers
*)
let solve1 (input : string) : int = lines input |> List.map decode |> sum

let part1 () : unit =
  read_file "input/day01.txt" |> solve1 |> Printf.printf "Part1: %d\n%!"
