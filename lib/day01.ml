(* lib/day01.ml *)

type part = One | Two

let digit_map : (string * char) list =
  [
    ("one", '1');
    ("two", '2');
    ("three", '3');
    ("four", '4');
    ("five", '5');
    ("six", '6');
    ("seven", '7');
    ("eight", '8');
    ("nine", '9');
  ]

let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let sum : int list -> int = List.fold_left ( + ) 0

let lines (input : string) : string list =
  input |> String.trim |> String.split_on_char '\n'

let is_digit (c : char) : bool = '0' <= c && c <= '9'
let to_digit (c : char) : int = Char.code c - Char.code '0'

let slice (str : string) (index : int) : string =
  let len = String.length str in
  (* todo: we'll never hit this condition because each line has digits *)
  if index < 0 || index > len then "" else String.sub str index (len - index)

let is_spelled (line : string) (index : int) : char option =
  let s =
    List.filter
      (fun (spelled, _) ->
        String.starts_with ~prefix:spelled (slice line index))
      digit_map
  in
  match s with
  | [ (_, digit) ] -> Some digit
  | _ -> None

let rec find_digit (line : string) (f : int -> int) (i : int) : int =
  if is_digit line.[i] then to_digit line.[i] else find_digit line f (f i)

let rec find_digit_spelled (line : string) (f : int -> int) (i : int) : int =
  if is_digit line.[i] then to_digit line.[i]
  else
    match is_spelled line i with
    | Some x -> to_digit x
    | None -> find_digit_spelled line f (f i)

(*
1. Split into lines
2. Find the first and last digit of each number
3. sum all numbers
*)
let decode (line : string) (part : part) : int =
  let inc x = x + 1 in
  let dec x = x - 1 in
  match part with
  | One ->
      let first_digit = find_digit line inc 0 in
      let last_digit = find_digit line dec (String.length line - 1) in
      (first_digit * 10) + last_digit
  | Two ->
      let first_digit = find_digit_spelled line inc 0 in
      let last_digit = find_digit_spelled line dec (String.length line - 1) in
      (first_digit * 10) + last_digit

let solve (input : string) (part : part) : int =
  lines input |> List.map (fun line -> decode line part) |> sum

let part1 () : unit =
  read_file "input/day01.txt" |> fun xs ->
  solve xs One |> Printf.printf "Part1: %d\n%!"

let part2 () : unit =
  read_file "input/day01.txt" |> fun xs ->
  solve xs Two |> Printf.printf "Part2: %d\n%!"
