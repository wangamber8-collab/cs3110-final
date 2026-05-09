open Types

(** A session tracks all scoring information for one play-through of the
    puzzle. All fields are immutable -- every update function returns a fresh
    record rather than mutating the existing one. *)
type session = {
  score : int;
  correct_count : int;
  wrong_count : int;
  hint_count : int;
  streak : int;
  max_streak : int;
}

(** A read-only end-of-game snapshot produced by [make_summary]. *)
type summary = {
  final_score : int;
  accuracy : float;
  grade : string;
  max_streak : int;
  hints_used : int;
}

(** Points deducted from the score for each wrong guess. *)
let wrong_penalty = 5

(** Points deducted from the score for each hint used. *)
let hint_penalty = 3

(** [make_session ()] returns a zeroed-out session to begin a new game. *)
let make_session () : session =
  {
    score = 0;
    correct_count = 0;
    wrong_count = 0;
    hint_count = 0;
    streak = 0;
    max_streak = 0;
  }

(** [base_points difficulty] is the base number of points for solving one node
    at the given difficulty level. Unrecognised strings default to easy. *)
let base_points (difficulty : string) : int =
  match difficulty with
  | "easy" -> 10
  | "medium" -> 20
  | "hard" -> 35
  | _ -> 10

(** [streak_bonus streak] is the bonus points earned when [streak] is a
    nonzero multiple of 3 -- i.e. every third consecutive correct answer earns
    an extra 15 points. Returns 0 otherwise. *)
let streak_bonus (streak : int) : int =
  if streak > 0 && streak mod 3 = 0 then 15 else 0

(** [score_for_node difficulty depth] is the total base point value of one
    correct answer at tree [depth] in a puzzle of [difficulty]. Root = depth 0.
    Each level deeper adds 5 bonus points because deeper nodes require more
    prerequisite answers to unlock. *)
let score_for_node (difficulty : string) (depth : int) : int =
  base_points difficulty + (depth * 5)

(** [node_depth root target] performs a DFS from [root] and returns [Some d]
    where [d] is the zero-indexed depth of [target] (root = 0), or [None] if
    [target] is not in the subtree. Physical equality (==) is used because
    nodes are mutable records -- each heap object has a unique identity. *)
let rec node_depth (root : node) (target : node) : int option =
  if root == target then Some 0
  else
    List.fold_left
      (fun acc child ->
        match acc with
        | Some _ -> acc
        | None -> Option.map (fun d -> d + 1) (node_depth child target))
      None root.children

(** [total_attempts s] is [s.correct_count + s.wrong_count]. Hints are not
    counted as answer attempts. *)
let total_attempts (s : session) : int = s.correct_count + s.wrong_count

(** [apply_correct s difficulty depth] returns a new session recording a
    correct answer. Awards [score_for_node difficulty depth] points plus a
    15-point streak bonus when the updated streak is a nonzero multiple of 3.
    Updates [streak] and [max_streak] accordingly. *)
let apply_correct (s : session) (difficulty : string) (depth : int) : session =
  let pts = score_for_node difficulty depth in
  let new_streak = s.streak + 1 in
  let bonus = streak_bonus new_streak in
  {
    score = s.score + pts + bonus;
    correct_count = s.correct_count + 1;
    wrong_count = s.wrong_count;
    hint_count = s.hint_count;
    streak = new_streak;
    max_streak = max s.max_streak new_streak;
  }

(** [apply_correct_from_puzzle s p n] is a convenience wrapper around
    [apply_correct] that automatically computes [n]'s depth in puzzle [p]'s
    tree. Defaults to depth 0 if [n] is not found in the tree. *)
let apply_correct_from_puzzle (s : session) (p : puzzle) (n : node) : session =
  let depth = Option.value ~default:0 (node_depth p.root n) in
  apply_correct s p.difficulty depth

(** [apply_wrong s] returns a new session recording a wrong guess. Deducts
    [wrong_penalty] points (score is floored at 0) and resets the current
    streak to 0. [max_streak] is preserved. *)
let apply_wrong (s : session) : session =
  {
    score = max 0 (s.score - wrong_penalty);
    correct_count = s.correct_count;
    wrong_count = s.wrong_count + 1;
    hint_count = s.hint_count;
    streak = 0;
    max_streak = s.max_streak;
  }

(** [apply_hint s] returns a new session recording one hint usage. Deducts
    [hint_penalty] points (score is floored at 0). Unlike a wrong guess, a
    hint does NOT reset the current streak. *)
let apply_hint (s : session) : session =
  {
    score = max 0 (s.score - hint_penalty);
    correct_count = s.correct_count;
    wrong_count = s.wrong_count;
    hint_count = s.hint_count + 1;
    streak = s.streak;
    max_streak = s.max_streak;
  }

(** [time_bonus elapsed_seconds] returns bonus points awarded for finishing
    the puzzle quickly. Tiers: < 60 s = 50 pts, < 120 s = 30 pts,
    < 180 s = 15 pts, 180 s or more = 0 pts. *)
let time_bonus (elapsed_seconds : int) : int =
  if elapsed_seconds < 0 then 0
  else if elapsed_seconds < 60 then 50
  else if elapsed_seconds < 120 then 30
  else if elapsed_seconds < 180 then 15
  else 0

(** [apply_time_bonus s elapsed_seconds] adds [time_bonus elapsed_seconds]
    to the session score. Intended to be called exactly once, when the puzzle
    is won. Does not affect any other session fields. *)
let apply_time_bonus (s : session) (elapsed_seconds : int) : session =
  { s with score = s.score + time_bonus elapsed_seconds }

(** [accuracy s] is the fraction of guesses that were correct, in [0.0, 1.0].
    Returns [0.0] when no guesses have been made to avoid division by zero. *)
let accuracy (s : session) : float =
  let total = total_attempts s in
  if total = 0 then 0.0
  else float_of_int s.correct_count /. float_of_int total

(** [is_perfect s] is [true] iff no wrong guesses were made and no hints were
    used during session [s]. A fresh session with zero guesses is perfect. *)
let is_perfect (s : session) : bool =
  s.wrong_count = 0 && s.hint_count = 0

(** [grade s] maps the session to a letter grade. A perfect run (no wrong
    guesses, no hints used) earns "S" regardless of score. Otherwise the grade
    follows accuracy: A >= 0.90, B >= 0.75, C >= 0.50, D below 0.50. *)
let grade (s : session) : string =
  if is_perfect s then "S"
  else
    let acc = accuracy s in
    if acc >= 0.9 then "A"
    else if acc >= 0.75 then "B"
    else if acc >= 0.5 then "C"
    else "D"

(** [make_summary s] captures the final state of [s] as an immutable [summary]
    record. Call once the puzzle is won to produce end-of-game statistics. *)
let make_summary (s : session) : summary =
  {
    final_score = s.score;
    accuracy = accuracy s;
    grade = grade s;
    max_streak = s.max_streak;
    hints_used = s.hint_count;
  }
