(* lib/day01.ml *)

type part = One | Two
type index = First | Last

let digit_map : (string * int) list =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let sum : int list -> int = List.fold_left ( + ) 0
let is_digit (c : char) : bool = '0' <= c && c <= '9'
let to_digit (c : char) : int = Char.code c - Char.code '0'

let slice (str : string) (index : int) : string =
  let len = String.length str in
  if index < 0 || index > len then "" else String.sub str index (len - index)

let is_spelled (line : string) (index : int) : int option =
  let s =
    List.filter
      (fun (spelled, _) ->
        String.starts_with ~prefix:spelled (slice line index))
      digit_map
  in
  match s with
  | [ (_, digit) ] -> Some digit
  | _ -> None

let find_digit (line : string) (part : part) (index : index) : int =
  let rec aux i f =
    match part with
    | One -> if is_digit line.[i] then to_digit line.[i] else aux (f i) f
    | Two -> (
        if is_digit line.[i] then to_digit line.[i]
        else
          match is_spelled line i with
          | Some x -> x
          | None -> aux (f i) f)
  in
  match index with
  | First -> aux 0 (fun x -> x + 1)
  | Last -> aux (String.length line - 1) (fun x -> x - 1)

(*
1. Split into lines
2. Find the first and last digit of each number
3. sum all numbers
*)
let decode (line : string) (part : part) : int =
  let combine first last = (first * 10) + last in
  combine (find_digit line part First) (find_digit line part Last)

let solve (lines: string list) (part : part) : int =
  List.map (fun line -> decode line part) lines |> sum

let solve_file () =
  let file_path = "input/day01.txt" in
  let xs = file_path |> Util.read_whole_file |> Util.split_lines in
  Printf.printf "Input file: %s\n" file_path;
  Printf.printf "Part1: %d\n%!" (solve xs One);
  Printf.printf "Part1: %d\n%!" (solve xs Two);
