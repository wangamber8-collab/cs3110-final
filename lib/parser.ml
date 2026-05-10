open Types
open Yojson.Basic.Util

let rec parse_node json =
  {
    label = json |> member "label" |> to_string;
    answer = json |> member "answer" |> to_string;
    children = json |> member "children" |> to_list |> List.map parse_node;
    solved = false;
  }

let parse_puzzle json =
  {
    id = json |> member "id" |> to_int;
    difficulty = json |> member "difficulty" |> to_string;
    theme = json |> member "theme" |> to_string;
    title = json |> member "title" |> to_string;
    solved_puzzle = false;
    root = json |> member "root" |> parse_node;
  }

let load_puzzles filepath =
  Yojson.Basic.from_file filepath |> to_list |> List.map parse_puzzle

let remaining_by_difficulty : (string, Types.puzzle list ref) Hashtbl.t =
  Hashtbl.create 8

let () = Random.self_init ()

let shuffle_list (lst : 'a list) : 'a list =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let fresh_queue_for_difficulty (difficulty : string)
    (puzzles : Types.puzzle list) : Types.puzzle list =
  puzzles
  |> List.filter (fun (p : Types.puzzle) -> p.difficulty = difficulty)
  |> shuffle_list

let choose_puzzle (difficulty : string) (puzzles : Types.puzzle list) :
    Types.puzzle option =
  let queue =
    match Hashtbl.find_opt remaining_by_difficulty difficulty with
    | Some q -> q
    | None ->
        let q = ref (fresh_queue_for_difficulty difficulty puzzles) in
        Hashtbl.add remaining_by_difficulty difficulty q;
        q
  in

  match !queue with
  | puzzle :: rest ->
      queue := rest;
      Some puzzle
  | [] -> (
      let fresh = fresh_queue_for_difficulty difficulty puzzles in
      queue := fresh;
      match !queue with
      | puzzle :: rest ->
          queue := rest;
          Some puzzle
      | [] -> None)
