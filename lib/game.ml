open Types

(* Types.node already carries a solved flag, so the parsed puzzle IS the game
   state — just alias the type for clarity in function signatures. *)
type state = puzzle

(* PURPOSE: canonicalize a string for answer comparison so "July", " July ", and
   "july" all match the same node. STEPS: 1. Trim leading/trailing whitespace.
   2. Uppercase every ASCII letter. NOTE: internal whitespace is preserved —
   answers like "Apollo 11" need the space between tokens. *)
let normalize (s : string) : string =
  let upper = String.uppercase_ascii s in
  String.trim upper

(* PURPOSE: split a label on {N} placeholders into alternating literal text and
   slot markers. The ONLY place that understands {N} syntax — the parser and
   Types treat labels as opaque strings. STEPS: 1. Scan the input for the
   pattern "{<digits>}". 2. Emit Text s for each stretch of literal text
   between/around placeholders. 3. Emit Slot i for each {i} encountered.
   EXAMPLE: "like most {0}lin and {1} movies" -> [ Text "like most "; Slot 0;
   Text "lin and "; Slot 1; Text " movies" ] NOTE: this does NOT recurse into
   children. Nesting is handled by render_node, which calls itself on
   children.(i) when it sees Slot i. A '{' not followed by digits+'}' is kept as
   literal text. Adjacent slots (e.g. "{0}{1}") produce NO empty Text between
   them. *)
let tokenize_label (label : string) : label_part list =
  let n = String.length label in
  (* scan_digits i: walk forward over ASCII digits starting at i. Returns
     (index_after_digits, digit_string). Empty string means no digits. *)
  let rec scan_digits i =
    if i < n && label.[i] >= '0' && label.[i] <= '9' then
      let e, s = scan_digits (i + 1) in
      (e, String.make 1 label.[i] ^ s)
    else (i, "")
  in
  (* Only emit Text when the buffer is non-empty — this is what keeps adjacent
     slots from producing a spurious Text "" between them. *)
  let flush buf acc = if buf = "" then acc else Text buf :: acc in
  let rec aux i buf acc =
    if i >= n then List.rev (flush buf acc)
    else if label.[i] = '{' then
      let j, digits = scan_digits (i + 1) in
      if digits <> "" && j < n && label.[j] = '}' then
        (* Valid {N}: flush text buffer, emit Slot, skip past '}'. *)
        aux (j + 1) "" (Slot (int_of_string digits) :: flush buf acc)
      else
        (* Stray '{' — treat as literal text and keep scanning. *)
        aux (i + 1) (buf ^ "{") acc
    else aux (i + 1) (buf ^ String.make 1 label.[i]) acc
  in
  aux 0 "" []

(* PURPOSE: render one node to its display string in the current state. Called
   recursively — a node's slots are filled by rendering its children. STEPS: 1.
   If n.solved -> return n.answer verbatim, WITHOUT brackets. 2. Else tokenize
   n.label and walk the token list, building a body: - Text s -> append s - Slot
   i -> append (render_node (List.nth n.children i)) 3. Wrap the body as "[" ^
   body ^ "]" and return. *)
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

(* PURPOSE: produce the full puzzle display string shown to the player. Calls
   render_node on the root node and returns the result directly.

   Since render_node now only adds brackets around currently answerable exposed
   nodes, render should not strip off the outermost characters. If the root is
   solved, render_node returns the root answer with no brackets. If the root is
   the current exposed node, render_node may return it bracketed so the frontend
   can highlight it. *)

(* let render (s : state) : string = if s.root.solved then s.root.answer else
   let full = render_node s.root in String.sub full 1 (String.length full -
   2) *)

let render (s : state) : string = render_node s.root

(* PURPOSE: collect every node the player can currently attempt — unsolved nodes
   whose children are ALL solved. At start these are the leaves. STEPS: 1. DFS
   from state.root. 2. At each unsolved node n: - if every child of n is solved
   -> include n (stop recursing here; the subtree below is fully solved). - else
   recurse into n's unsolved children to find deeper exposed nodes. 3. Return
   the collected list. *)
let exposed (s : state) : node list =
  let rec find n =
    if n.solved then []
    else if List.for_all (fun c -> c.solved) n.children then [ n ]
    else List.concat_map find n.children
  in
  find s.root

(* PURPOSE: accept a user answer; if it matches an exposed node's answer, return
   a modified state (switch the mutable flag solved) with that node flipped to
   solved. ASSUMPTION: puzzle answers are unique across the whole tree, so at
   most one node can match a given input. STEPS: 1. Let input' = normalize
   input. 2. Find the unique node n where: - n is exposed (unsolved, all
   children solved), AND - normalize n.answer = input'. 3. If found -> return
   true. If not found -> return false. *)
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

(* checks if the player has won the game by checking if the root node is
   solved*)
let is_won (_s : state) : bool =
  let result = _s.root.solved in
  if result then _s.solved_puzzle <- true else ();
  result

(* Exposed for testing only — see game.mli *)
let rec count_nodes (n : node) : int =
  1 + List.fold_left (fun acc child -> acc + count_nodes child) 0 n.children

(* Exposed for testing only — see game.mli *)
let rec count_solved (n : node) : int =
  let self = if n.solved then 1 else 0 in
  self + List.fold_left (fun acc child -> acc + count_solved child) 0 n.children

(* PURPOSE: return the player's current progress through the puzzle as
   (solved_count, total_count). STEPS: 1. Count all nodes in the tree via
   count_nodes. 2. Count solved nodes via count_solved. 3. Return the pair. *)
let progress (s : state) : int * int = (count_solved s.root, count_nodes s.root)

(* PURPOSE: given the inner text of a clicked chip (the body inside [...]), find
   the matching exposed node and return the first character of its answer.
   Returns None if no exposed node matches the body or the answer is empty.
   NOTE: exposed nodes are always wrapped in [...] by render_node, so we strip
   the outer brackets to get the body for comparison. This is for first click
   behavior*)
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

(* PURPOSE: remove one pair of outer square brackets from a string, if they are
   present. If [s] starts with "[" and ends with "]", returns the inside of the
   brackets. Otherwise, returns [s] unchanged.

   EXAMPLES: strip_outer_brackets "[hello]" = "hello"*)
let strip_outer_brackets (s : string) : string =
  let len = String.length s in
  if len >= 2 && s.[0] = '[' && s.[len - 1] = ']' then String.sub s 1 (len - 2)
  else s

(* PURPOSE: compute the clickable/display body of an exposed node. Since
   render_node wraps exposed nodes in square brackets, this first renders the
   node and then removes the outer brackets.

   This lets us compare the text the frontend sends back after a user clicks a
   bracket chip with the corresponding exposed node in the OCaml game state. *)
let body_of_exposed_node (n : node) : string =
  render_node n |> strip_outer_brackets

(* PURPOSE: reveal a currently answerable bracket based on the text inside the
   clicked chip.

   [chip_body] is the text inside the clicked bracket, without the outer square
   brackets. The function searches through the currently exposed nodes for a
   node whose rendered body matches [chip_body]. If it finds one, it marks that
   node as solved and returns true. If no exposed node matches, it returns
   false.

   This is used for the second-click hint behavior: first click gives the first
   letter, second click reveals/solves the bracket. *)
let reveal_by_body (chip_body : string) (s : state) : bool =
  match
    List.find_opt (fun n -> body_of_exposed_node n = chip_body) (exposed s)
  with
  | None -> false
  | Some n ->
      n.solved <- true;
      true

(* PURPOSE: given the inner text of a clicked chip, find the matching exposed
   node and return the character count of its answer. Returns None if no
   exposed node matches or the answer is empty. Used to let the player know
   how many characters to type before committing a guess. *)
let hint_answer_length (chip_body : string) (s : state) : int option =
  match List.find_opt (fun n -> body_of_exposed_node n = chip_body) (exposed s) with
  | None -> None
  | Some n ->
      let len = String.length n.answer in
      if len = 0 then None else Some len

(* PURPOSE: given the inner text of a clicked chip, find the matching exposed
   node and return the number of words in its answer. Words are separated by
   one or more space characters. Returns None if no exposed node matches or
   the answer is empty. Useful for multi-word answers where length alone is
   not enough information. *)
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
