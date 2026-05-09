open Types

type state = puzzle

let normalize (s : string) : string =
  let upper = String.uppercase_ascii s in
  String.trim upper

let tokenize_label (label : string) : label_part list =
  let n = String.length label in
  let rec scan_digits i =
    if i < n && label.[i] >= '0' && label.[i] <= '9' then
      let e, s = scan_digits (i + 1) in
      (e, String.make 1 label.[i] ^ s)
    else (i, "")
  in
  let flush buf acc = if buf = "" then acc else Text buf :: acc in
  let rec aux i buf acc =
    if i >= n then List.rev (flush buf acc)
    else if label.[i] = '{' then
      let j, digits = scan_digits (i + 1) in
      if digits <> "" && j < n && label.[j] = '}' then
        aux (j + 1) "" (Slot (int_of_string digits) :: flush buf acc)
      else
        aux (i + 1) (buf ^ "{") acc
    else aux (i + 1) (buf ^ String.make 1 label.[i]) acc
  in
  aux 0 "" []

let rec render_node (n : node) : string =
  if n.solved then n.answer
  else
    let part_to_string = function
      | Text s -> s
      | Slot i -> render_node (List.nth n.children i)
    in
    let body =
      tokenize_label n.label |> List.map part_to_string |> String.concat ""
    in
    if List.for_all (fun c -> c.solved) n.children then "[" ^ body ^ "]"
    else body

let render (s : state) : string = render_node s.root

let exposed (s : state) : node list =
  let rec find n =
    if n.solved then []
    else if List.for_all (fun c -> c.solved) n.children then [ n ]
    else List.concat_map find n.children
  in
  find s.root

let submit (user_input : string) (game_s : state) : bool =
  let can_be_solved : node list = exposed game_s in

  let len = List.length can_be_solved in

  let corrected_input = normalize user_input in

  let rec find_match index input =
    if index < len then
      if normalize (List.nth can_be_solved index).answer = corrected_input then (
        (List.nth can_be_solved index).solved <- true;
        true)
      else find_match (index + 1) input
    else false
  in
  find_match 0 corrected_input

let is_won (_s : state) : bool =
  let result = _s.root.solved in
  if result then _s.solved_puzzle <- true else ();
  result

let rec count_nodes (n : node) : int =
  1 + List.fold_left (fun acc child -> acc + count_nodes child) 0 n.children

let rec count_solved (n : node) : int =
  let self = if n.solved then 1 else 0 in
  self + List.fold_left (fun acc child -> acc + count_solved child) 0 n.children

let progress (s : state) : int * int = (count_solved s.root, count_nodes s.root)

let hint_first_letter (chip_body : string) (s : state) : string option =
  let body_of n =
    let r = render_node n in
    String.sub r 1 (String.length r - 2)
  in
  match List.find_opt (fun n -> body_of n = chip_body) (exposed s) with
  | None -> None
  | Some n ->
      if String.length n.answer > 0 then Some (String.make 1 n.answer.[0])
      else None

let strip_outer_brackets (s : string) : string =
  let len = String.length s in
  if len >= 2 && s.[0] = '[' && s.[len - 1] = ']' then String.sub s 1 (len - 2)
  else s

let body_of_exposed_node (n : node) : string =
  render_node n |> strip_outer_brackets

let reveal_by_body (chip_body : string) (s : state) : bool =
  match
    List.find_opt (fun n -> body_of_exposed_node n = chip_body) (exposed s)
  with
  | None -> false
  | Some n ->
      n.solved <- true;
      true

let hint_answer_length (chip_body : string) (s : state) : int option =
  match List.find_opt (fun n -> body_of_exposed_node n = chip_body) (exposed s) with
  | None -> None
  | Some n ->
      let len = String.length n.answer in
      if len = 0 then None else Some len

let hint_word_count (chip_body : string) (s : state) : int option =
  match List.find_opt (fun n -> body_of_exposed_node n = chip_body) (exposed s) with
  | None -> None
  | Some n ->
      let trimmed = String.trim n.answer in
      if String.length trimmed = 0 then None
      else
        let words =
          String.split_on_char ' ' trimmed
          |> List.filter (fun w -> String.length w > 0)
        in
        Some (List.length words)
