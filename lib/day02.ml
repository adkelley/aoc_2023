(* lib/day02.ml *)

type part = One | Two

let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let sum : int list -> int = List.fold_left ( + ) 0

let lines (input : string) : string list =
  input |> String.trim |> String.split_on_char '\n'

(* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green -> (1, "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") *)
let parse_game_id (line : string) : (int * string) option =
  let parse_id (first : string) : int option =
    String.split_on_char ' ' first |> fun xs ->
    match xs with
    | _ :: id :: _ -> Some (int_of_string id)
    | _ -> None
  in
  line |> String.split_on_char ':' |> List.map String.trim |> fun xs ->
  match xs with
  | first :: rest :: _ -> (
      match parse_id first with
      | Some id -> Some (id, rest)
      | None -> None)
  | _ -> None

type set = { red : int; green : int; blue : int }

let init_set () : set = { red = 0; green = 0; blue = 0 }

(* "3 blue" -> {red: 0, green: 0, blue: 3} *)
let parse_cube (s : set) (cs : string) : set =
  cs |> String.split_on_char ' ' |> List.map String.trim |> fun xs ->
  match xs with
  | num :: "red" :: _ ->
      { red = int_of_string num; green = s.green; blue = s.blue }
  | num :: "green" :: _ ->
      { red = s.red; green = int_of_string num; blue = s.blue }
  | num :: "blue" :: _ ->
      { red = s.red; green = s.green; blue = int_of_string num }
  | _ -> s

(* " 3 blue, 4 red" -> {red: 4, blue: 3, green: 0} *)
let parse_cubes (cubes : string) : set =
  let init = init_set () in
  cubes
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.fold_left (fun (s : set) (cs : string) : set -> parse_cube s cs) init

(* " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> [{red: 4, blue: 3, green: 0}]*)
let parse_sets (set : string) : set list =
  let parse_set (s : string) : set = parse_cubes s in
  set |> String.split_on_char ';' |> List.map String.trim |> List.map (fun s -> parse_set s)

let solve (input : string) (_part : part) : (int * set list) option list =
  lines input
  |> List.map (fun xs -> parse_game_id xs)
  |> List.map (fun xs ->
         match xs with
         | Some (id, sets) -> Some (id, parse_sets sets)
         | None -> None)

let print_sets (sets : set list) : string =
  List.fold_left
    (fun (c : string) (s : set) : string ->
      c
      ^ "red: "
      ^ string_of_int s.red
      ^ " green: "
      ^ string_of_int s.green
      ^ " blue: "
      ^ string_of_int s.blue
      ^ ";")
    " " sets

let part1 () : unit =
  read_file "input/day02.txt" |> fun xs ->
  solve xs One
  |> List.iter (fun xs ->
         match xs with
         | Some (id, sets) -> Printf.printf "Game: %d %s\n" id (print_sets sets)
         | None -> print_endline "failed to parse Game")

(*
let part2 () : unit =
  read_file "input/day02.txt" |> fun xs ->
  solve xs Two |> Printf.printf "Part2: %d\n%!"
*)
