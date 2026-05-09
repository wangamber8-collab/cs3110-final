open OUnit2
open Cs3110_final.Types
open Cs3110_final.Score
open Cs3110_final.Game
open Cs3110_final.Parser

(** Shorthand for a fresh zero session. *)
let fresh () = make_session ()

(** Build a minimal one-node puzzle with the given difficulty. *)
let solo_puzzle diff answer =
  let root = { label = answer; answer; children = []; solved = false } in
  { id = 0; difficulty = diff; theme = ""; title = ""; solved_puzzle = false; root }

let tests =
  "score test suite"
  >::: [
    (* ── make_session ─────────────────────────────────────────────────── *)
    ( "make_session: score starts at 0" >:: fun _ ->
      assert_equal 0 (fresh ()).score );
    ( "make_session: correct_count starts at 0" >:: fun _ ->
      assert_equal 0 (fresh ()).correct_count );
    ( "make_session: wrong_count starts at 0" >:: fun _ ->
      assert_equal 0 (fresh ()).wrong_count );
    ( "make_session: hint_count starts at 0" >:: fun _ ->
      assert_equal 0 (fresh ()).hint_count );
    ( "make_session: streak starts at 0" >:: fun _ ->
      assert_equal 0 (fresh ()).streak );
    ( "make_session: max_streak starts at 0" >:: fun _ ->
      assert_equal 0 (fresh ()).max_streak );
    ( "make_session: two calls produce independent records" >:: fun _ ->
      let s1 = fresh () in
      let s2 = fresh () in
      assert_equal s1.score s2.score );

    (* ── score_for_node ───────────────────────────────────────────────── *)
    ( "score_for_node: easy at depth 0 gives 10" >:: fun _ ->
      assert_equal 10 (score_for_node "easy" 0) );
    ( "score_for_node: medium at depth 0 gives 20" >:: fun _ ->
      assert_equal 20 (score_for_node "medium" 0) );
    ( "score_for_node: hard at depth 0 gives 35" >:: fun _ ->
      assert_equal 35 (score_for_node "hard" 0) );
    ( "score_for_node: unrecognised difficulty defaults to easy (10)" >:: fun _ ->
      assert_equal 10 (score_for_node "legendary" 0) );
    ( "score_for_node: easy at depth 1 gives 15" >:: fun _ ->
      assert_equal 15 (score_for_node "easy" 1) );
    ( "score_for_node: easy at depth 2 gives 20" >:: fun _ ->
      assert_equal 20 (score_for_node "easy" 2) );
    ( "score_for_node: hard at depth 3 gives 50" >:: fun _ ->
      assert_equal 50 (score_for_node "hard" 3) );
    ( "score_for_node: medium at depth 4 gives 40" >:: fun _ ->
      assert_equal 40 (score_for_node "medium" 4) );
    ( "score_for_node: depth 0 adds no bonus regardless of difficulty" >:: fun _ ->
      assert_equal 35 (score_for_node "hard" 0) );

    (* ── node_depth ───────────────────────────────────────────────────── *)
    ( "node_depth: root returns Some 0" >:: fun _ ->
      let n = { label = ""; answer = "A"; children = []; solved = false } in
      assert_equal (Some 0) (node_depth n n) );
    ( "node_depth: direct child returns Some 1" >:: fun _ ->
      let child = { label = ""; answer = "B"; children = []; solved = false } in
      let root = { label = ""; answer = "A"; children = [child]; solved = false } in
      assert_equal (Some 1) (node_depth root child) );
    ( "node_depth: grandchild returns Some 2" >:: fun _ ->
      let gc = { label = ""; answer = "C"; children = []; solved = false } in
      let child = { label = ""; answer = "B"; children = [gc]; solved = false } in
      let root = { label = ""; answer = "A"; children = [child]; solved = false } in
      assert_equal (Some 2) (node_depth root gc) );
    ( "node_depth: node not in tree returns None" >:: fun _ ->
      let other = { label = ""; answer = "Z"; children = []; solved = false } in
      let root = { label = ""; answer = "A"; children = []; solved = false } in
      assert_equal None (node_depth root other) );
    ( "node_depth: finds the second child at depth 1" >:: fun _ ->
      let c1 = { label = ""; answer = "B"; children = []; solved = false } in
      let c2 = { label = ""; answer = "C"; children = []; solved = false } in
      let root =
        { label = ""; answer = "A"; children = [c1; c2]; solved = false }
      in
      assert_equal (Some 1) (node_depth root c2) );
    ( "node_depth: distinguishes two nodes with identical labels via physical eq"
    >:: fun _ ->
      let c1 = { label = "x"; answer = "X"; children = []; solved = false } in
      let c2 = { label = "x"; answer = "X"; children = []; solved = false } in
      let root =
        { label = ""; answer = "A"; children = [c1; c2]; solved = false }
      in
      assert_equal (Some 1) (node_depth root c1);
      assert_equal (Some 1) (node_depth root c2) );

    (* ── total_attempts ───────────────────────────────────────────────── *)
    ( "total_attempts: 0 on a fresh session" >:: fun _ ->
      assert_equal 0 (total_attempts (fresh ())) );
    ( "total_attempts: counts one correct guess" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      assert_equal 1 (total_attempts s) );
    ( "total_attempts: counts one wrong guess" >:: fun _ ->
      let s = apply_wrong (fresh ()) in
      assert_equal 1 (total_attempts s) );
    ( "total_attempts: hints do not count as attempts" >:: fun _ ->
      let s = apply_hint (fresh ()) in
      assert_equal 0 (total_attempts s) );
    ( "total_attempts: sums correct and wrong independently of hints" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_hint s1 in
      let s3 = apply_wrong s2 in
      let s4 = apply_correct s3 "medium" 1 in
      assert_equal 3 (total_attempts s4) );

    (* ── apply_correct: score ─────────────────────────────────────────── *)
    ( "apply_correct: easy depth 0 awards 10 points" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      assert_equal 10 s.score );
    ( "apply_correct: medium depth 0 awards 20 points" >:: fun _ ->
      let s = apply_correct (fresh ()) "medium" 0 in
      assert_equal 20 s.score );
    ( "apply_correct: hard depth 0 awards 35 points" >:: fun _ ->
      let s = apply_correct (fresh ()) "hard" 0 in
      assert_equal 35 s.score );
    ( "apply_correct: easy depth 1 awards 15 points" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 1 in
      assert_equal 15 s.score );
    ( "apply_correct: scores accumulate across two calls" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "medium" 0 in
      assert_equal 30 s2.score );
    ( "apply_correct: does not mutate the previous session record" >:: fun _ ->
      let s0 = fresh () in
      let _s1 = apply_correct s0 "easy" 0 in
      assert_equal 0 s0.score );

    (* ── apply_correct: streak and streak bonus ───────────────────────── *)
    ( "apply_correct: streak becomes 1 after first correct" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      assert_equal 1 s.streak );
    ( "apply_correct: streak becomes 2 after two consecutive corrects" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      assert_equal 2 s2.streak );
    ( "apply_correct: no streak bonus at streak 2" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      assert_equal 20 s2.score );
    ( "apply_correct: 15-point streak bonus fires at streak 3" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      (* 10 + 10 + 10 + 15 bonus = 45 *)
      assert_equal 45 s3.score );
    ( "apply_correct: no bonus at streak 4" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_correct s3 "easy" 0 in
      (* 45 + 10 = 55 *)
      assert_equal 55 s4.score );
    ( "apply_correct: second bonus fires at streak 6" >:: fun _ ->
      let s = ref (fresh ()) in
      for _ = 1 to 6 do
        s := apply_correct !s "easy" 0
      done;
      (* 6 * 10 + 15 (at 3) + 15 (at 6) = 90 *)
      assert_equal 90 !s.score );
    ( "apply_correct: third bonus fires at streak 9" >:: fun _ ->
      let s = ref (fresh ()) in
      for _ = 1 to 9 do
        s := apply_correct !s "easy" 0
      done;
      (* 9 * 10 + 15 * 3 = 135 *)
      assert_equal 135 !s.score );
    ( "apply_correct: streak resets to 1 after an intervening wrong guess"
    >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_wrong s2 in
      let s4 = apply_correct s3 "easy" 0 in
      assert_equal 1 s4.streak );

    (* ── apply_correct: max_streak ───────────────────────────────────── *)
    ( "apply_correct: max_streak tracks the peak streak" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      assert_equal 3 s3.max_streak );
    ( "apply_correct: max_streak preserved after streak resets" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_wrong s3 in
      let s5 = apply_correct s4 "easy" 0 in
      assert_equal 3 s5.max_streak );
    ( "apply_correct: max_streak updates when new streak beats old record"
    >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_wrong s1 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_correct s3 "easy" 0 in
      let s5 = apply_correct s4 "easy" 0 in
      let s6 = apply_correct s5 "easy" 0 in
      assert_equal 4 s6.max_streak );

    (* ── apply_correct: counts ───────────────────────────────────────── *)
    ( "apply_correct: increments correct_count by 1 per call" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "medium" 1 in
      assert_equal 2 s2.correct_count );
    ( "apply_correct: does not change wrong_count" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      assert_equal 0 s.wrong_count );
    ( "apply_correct: does not change hint_count" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      assert_equal 0 s.hint_count );

    (* ── apply_wrong: score ──────────────────────────────────────────── *)
    ( "apply_wrong: deducts wrong_penalty from a positive score" >:: fun _ ->
      let s0 = apply_correct (fresh ()) "easy" 0 in
      let s1 = apply_wrong s0 in
      assert_equal 5 s1.score );
    ( "apply_wrong: floors score at 0 when session is empty" >:: fun _ ->
      let s = apply_wrong (fresh ()) in
      assert_equal 0 s.score );
    ( "apply_wrong: multiple wrongs on a zero score stay at 0" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_wrong s0 in
      let s2 = apply_wrong s1 in
      let s3 = apply_wrong s2 in
      assert_equal 0 s3.score );
    ( "apply_wrong: floors at 0 when score falls just below penalty" >:: fun _ ->
      let s0 = apply_correct (fresh ()) "easy" 0 in
      (* score = 10 *)
      let s1 = apply_wrong s0 in
      (* score = 5 *)
      let s2 = apply_wrong s1 in
      (* score = 0 *)
      let s3 = apply_wrong s2 in
      (* stays 0 *)
      assert_equal 0 s3.score );

    (* ── apply_wrong: streak ─────────────────────────────────────────── *)
    ( "apply_wrong: resets streak to 0" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_wrong s2 in
      assert_equal 0 s3.streak );
    ( "apply_wrong: preserves max_streak" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_wrong s3 in
      assert_equal 3 s4.max_streak );

    (* ── apply_wrong: counts ─────────────────────────────────────────── *)
    ( "apply_wrong: increments wrong_count" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_wrong s0 in
      let s2 = apply_wrong s1 in
      assert_equal 2 s2.wrong_count );
    ( "apply_wrong: does not change correct_count" >:: fun _ ->
      let s = apply_wrong (fresh ()) in
      assert_equal 0 s.correct_count );
    ( "apply_wrong: does not change hint_count" >:: fun _ ->
      let s = apply_wrong (fresh ()) in
      assert_equal 0 s.hint_count );

    (* ── apply_hint: score ───────────────────────────────────────────── *)
    ( "apply_hint: deducts hint_penalty from a positive score" >:: fun _ ->
      let s0 = apply_correct (fresh ()) "easy" 0 in
      let s1 = apply_hint s0 in
      assert_equal 7 s1.score );
    ( "apply_hint: floors score at 0 on an empty session" >:: fun _ ->
      let s = apply_hint (fresh ()) in
      assert_equal 0 s.score );
    ( "apply_hint: stacks correctly across multiple uses" >:: fun _ ->
      let s0 = apply_correct (fresh ()) "medium" 0 in
      (* score = 20 *)
      let s1 = apply_hint s0 in
      (* score = 17 *)
      let s2 = apply_hint s1 in
      (* score = 14 *)
      assert_equal 14 s2.score );
    ( "apply_hint: hint_penalty is smaller than wrong_penalty" >:: fun _ ->
      assert_bool "hint_penalty should be less than wrong_penalty"
        (hint_penalty < wrong_penalty) );

    (* ── apply_hint: streak ──────────────────────────────────────────── *)
    ( "apply_hint: does not reset streak" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_hint s2 in
      assert_equal 2 s3.streak );
    ( "apply_hint: does not affect max_streak" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_hint s2 in
      assert_equal 2 s3.max_streak );
    ( "apply_hint: streak bonus still fires after a hint mid-streak" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      (* streak 1, score 10 *)
      let s2 = apply_hint s1 in
      (* streak 1, score 7 *)
      let s3 = apply_correct s2 "easy" 0 in
      (* streak 2, score 17 *)
      let s4 = apply_correct s3 "easy" 0 in
      (* streak 3 -> +15 bonus, score 42 *)
      assert_equal 42 s4.score );

    (* ── apply_hint: counts ──────────────────────────────────────────── *)
    ( "apply_hint: increments hint_count" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_hint s0 in
      let s2 = apply_hint s1 in
      let s3 = apply_hint s2 in
      assert_equal 3 s3.hint_count );
    ( "apply_hint: does not change correct_count" >:: fun _ ->
      let s = apply_hint (fresh ()) in
      assert_equal 0 s.correct_count );
    ( "apply_hint: does not change wrong_count" >:: fun _ ->
      let s = apply_hint (fresh ()) in
      assert_equal 0 s.wrong_count );

    (* ── time_bonus ──────────────────────────────────────────────────── *)
    ( "time_bonus: returns 50 for 0 seconds" >:: fun _ ->
      assert_equal 50 (time_bonus 0) );
    ( "time_bonus: returns 50 for 59 seconds" >:: fun _ ->
      assert_equal 50 (time_bonus 59) );
    ( "time_bonus: returns 30 for exactly 60 seconds" >:: fun _ ->
      assert_equal 30 (time_bonus 60) );
    ( "time_bonus: returns 30 for 119 seconds" >:: fun _ ->
      assert_equal 30 (time_bonus 119) );
    ( "time_bonus: returns 15 for exactly 120 seconds" >:: fun _ ->
      assert_equal 15 (time_bonus 120) );
    ( "time_bonus: returns 15 for 179 seconds" >:: fun _ ->
      assert_equal 15 (time_bonus 179) );
    ( "time_bonus: returns 0 for exactly 180 seconds" >:: fun _ ->
      assert_equal 0 (time_bonus 180) );
    ( "time_bonus: returns 0 for very large values" >:: fun _ ->
      assert_equal 0 (time_bonus 9999) );
    ( "time_bonus: returns 0 for negative elapsed time" >:: fun _ ->
      assert_equal 0 (time_bonus (-1)) );

    (* ── apply_time_bonus ────────────────────────────────────────────── *)
    ( "apply_time_bonus: adds correct bonus for fast completion" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      (* score = 10 *)
      let s2 = apply_time_bonus s 30 in
      (* score = 10 + 50 = 60 *)
      assert_equal 60 s2.score );
    ( "apply_time_bonus: does not affect any count or streak fields" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_time_bonus s1 30 in
      assert_equal s1.correct_count s2.correct_count;
      assert_equal s1.wrong_count s2.wrong_count;
      assert_equal s1.hint_count s2.hint_count;
      assert_equal s1.streak s2.streak;
      assert_equal s1.max_streak s2.max_streak );
    ( "apply_time_bonus: adds 0 for slow completion" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      let s2 = apply_time_bonus s 300 in
      assert_equal 10 s2.score );

    (* ── accuracy ────────────────────────────────────────────────────── *)
    ( "accuracy: 0.0 on a fresh session with no guesses" >:: fun _ ->
      assert_equal 0.0 (accuracy (fresh ())) );
    ( "accuracy: 1.0 when all guesses are correct" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "hard" 2 in
      assert_equal 1.0 (accuracy s2) );
    ( "accuracy: 0.0 when all guesses are wrong" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_wrong s0 in
      let s2 = apply_wrong s1 in
      assert_equal 0.0 (accuracy s2) );
    ( "accuracy: 0.5 for one correct and one wrong" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_wrong s1 in
      assert_equal 0.5 (accuracy s2) );
    ( "accuracy: 0.75 for three correct and one wrong" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_wrong s3 in
      assert_equal 0.75 (accuracy s4) );
    ( "accuracy: 0.25 for one correct and three wrong" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_wrong s1 in
      let s3 = apply_wrong s2 in
      let s4 = apply_wrong s3 in
      assert_equal 0.25 (accuracy s4) );
    ( "accuracy: hints do not count as attempts" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_hint s1 in
      let s3 = apply_hint s2 in
      assert_equal 1.0 (accuracy s3) );

    (* ── is_perfect ──────────────────────────────────────────────────── *)
    ( "is_perfect: true on a fresh session" >:: fun _ ->
      assert_equal true (is_perfect (fresh ())) );
    ( "is_perfect: true after only correct answers" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "hard" 2 in
      assert_equal true (is_perfect s2) );
    ( "is_perfect: false after one wrong guess" >:: fun _ ->
      let s = apply_wrong (fresh ()) in
      assert_equal false (is_perfect s) );
    ( "is_perfect: false after one hint" >:: fun _ ->
      let s = apply_hint (fresh ()) in
      assert_equal false (is_perfect s) );
    ( "is_perfect: false after wrong then correct" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_wrong s0 in
      let s2 = apply_correct s1 "easy" 0 in
      assert_equal false (is_perfect s2) );
    ( "is_perfect: false after hint then correct" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_hint s0 in
      let s2 = apply_correct s1 "easy" 0 in
      assert_equal false (is_perfect s2) );

    (* ── grade ───────────────────────────────────────────────────────── *)
    ( "grade: S on a fresh session (perfect, zero guesses)" >:: fun _ ->
      assert_equal "S" (grade (fresh ())) );
    ( "grade: S for all-correct session with no hints" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "hard" 2 in
      assert_equal "S" (grade s2) );
    ( "grade: A for >= 90% accuracy with at least one wrong" >:: fun _ ->
      let s = ref (fresh ()) in
      for _ = 1 to 9 do
        s := apply_correct !s "easy" 0
      done;
      s := apply_wrong !s;
      assert_equal "A" (grade !s) );
    ( "grade: B for exactly 75% accuracy" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_wrong s3 in
      assert_equal "B" (grade s4) );
    ( "grade: C for exactly 50% accuracy" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_wrong s1 in
      assert_equal "C" (grade s2) );
    ( "grade: D for below 50% accuracy" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_wrong s1 in
      let s3 = apply_wrong s2 in
      let s4 = apply_wrong s3 in
      assert_equal "D" (grade s4) );
    ( "grade: not S when hints were used even with perfect guesses" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_hint s1 in
      assert_bool "grade should not be S after a hint" (grade s2 <> "S") );
    ( "grade: A for 100% guess accuracy when hints were used" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_hint s0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      assert_equal "A" (grade s3) );

    (* ── make_summary ────────────────────────────────────────────────── *)
    ( "make_summary: final_score matches session score" >:: fun _ ->
      let s = apply_correct (fresh ()) "hard" 2 in
      let sum = make_summary s in
      assert_equal s.score sum.final_score );
    ( "make_summary: accuracy matches computed accuracy" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_wrong s1 in
      let sum = make_summary s2 in
      assert_equal 0.5 sum.accuracy );
    ( "make_summary: grade matches computed grade" >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      let sum = make_summary s in
      assert_equal "S" sum.grade );
    ( "make_summary: max_streak matches session max_streak" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_correct s0 "easy" 0 in
      let s2 = apply_correct s1 "easy" 0 in
      let s3 = apply_correct s2 "easy" 0 in
      let s4 = apply_wrong s3 in
      let s5 = apply_correct s4 "easy" 0 in
      let sum = make_summary s5 in
      assert_equal 3 sum.max_streak );
    ( "make_summary: hints_used matches hint_count" >:: fun _ ->
      let s0 = fresh () in
      let s1 = apply_hint s0 in
      let s2 = apply_hint s1 in
      let sum = make_summary s2 in
      assert_equal 2 sum.hints_used );

    (* ── apply_correct_from_puzzle ───────────────────────────────────── *)
    ( "apply_correct_from_puzzle: awards depth-0 points for the root node"
    >:: fun _ ->
      let p = solo_puzzle "easy" "Apple" in
      let s = apply_correct_from_puzzle (fresh ()) p p.root in
      assert_equal 10 s.score );
    ( "apply_correct_from_puzzle: awards depth-1 bonus for a direct child"
    >:: fun _ ->
      let child =
        { label = ""; answer = "Child"; children = []; solved = false }
      in
      let root =
        { label = ""; answer = "Root"; children = [child]; solved = false }
      in
      let p =
        {
          id = 1;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root;
        }
      in
      let s = apply_correct_from_puzzle (fresh ()) p child in
      assert_equal 15 s.score );
    ( "apply_correct_from_puzzle: uses the puzzle's difficulty field" >:: fun _ ->
      let p = solo_puzzle "hard" "Hard answer" in
      let s = apply_correct_from_puzzle (fresh ()) p p.root in
      assert_equal 35 s.score );
    ( "apply_correct_from_puzzle: defaults to depth 0 for a node not in tree"
    >:: fun _ ->
      let p = solo_puzzle "easy" "Apple" in
      let stranger =
        { label = ""; answer = "Z"; children = []; solved = false }
      in
      let s = apply_correct_from_puzzle (fresh ()) p stranger in
      assert_equal 10 s.score );

    (* ── hint_answer_length (via Game) ───────────────────────────────── *)
    ( "hint_answer_length: returns Some with correct length for a leaf node"
    >:: fun _ ->
      let leaf =
        { label = "a clue"; answer = "Apple"; children = []; solved = false }
      in
      let p =
        {
          id = 0;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root =
            {
              label = "{0}";
              answer = "Apple";
              children = [ leaf ];
              solved = false;
            };
        }
      in
      assert_equal (Some 5) (hint_answer_length "a clue" p) );
    ( "hint_answer_length: returns None for a body that matches no exposed node"
    >:: fun _ ->
      let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json" in
      let p = List.hd puzzles in
      assert_equal None (hint_answer_length "this matches nothing" p) );
    ( "hint_answer_length: returns None when the node is already solved"
    >:: fun _ ->
      let leaf =
        { label = "a clue"; answer = "Apple"; children = []; solved = true }
      in
      let p =
        {
          id = 0;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root =
            {
              label = "{0}";
              answer = "Apple";
              children = [ leaf ];
              solved = false;
            };
        }
      in
      assert_equal None (hint_answer_length "a clue" p) );
    ( "hint_answer_length: works on a real loaded puzzle exposed node" >:: fun _ ->
      let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json" in
      let p = List.hd puzzles in
      let first_exposed = List.hd (exposed p) in
      let body =
        let r = render_node first_exposed in
        String.sub r 1 (String.length r - 2)
      in
      let expected = Some (String.length first_exposed.answer) in
      assert_equal expected (hint_answer_length body p) );

    (* ── hint_word_count (via Game) ──────────────────────────────────── *)
    ( "hint_word_count: returns Some 1 for a single-word answer" >:: fun _ ->
      let leaf =
        { label = "a clue"; answer = "Apple"; children = []; solved = false }
      in
      let p =
        {
          id = 0;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root =
            {
              label = "{0}";
              answer = "Apple";
              children = [ leaf ];
              solved = false;
            };
        }
      in
      assert_equal (Some 1) (hint_word_count "a clue" p) );
    ( "hint_word_count: returns Some 2 for a two-word answer" >:: fun _ ->
      let leaf =
        {
          label = "space clue";
          answer = "Apollo 11";
          children = [];
          solved = false;
        }
      in
      let p =
        {
          id = 0;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root =
            {
              label = "{0}";
              answer = "Apollo 11";
              children = [ leaf ];
              solved = false;
            };
        }
      in
      assert_equal (Some 2) (hint_word_count "space clue" p) );
    ( "hint_word_count: handles leading and trailing whitespace in the answer"
    >:: fun _ ->
      let leaf =
        {
          label = "padded";
          answer = "  hello world  ";
          children = [];
          solved = false;
        }
      in
      let p =
        {
          id = 0;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root =
            {
              label = "{0}";
              answer = "  hello world  ";
              children = [ leaf ];
              solved = false;
            };
        }
      in
      assert_equal (Some 2) (hint_word_count "padded" p) );
    ( "hint_word_count: returns None for body that matches no exposed node"
    >:: fun _ ->
      let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json" in
      let p = List.hd puzzles in
      assert_equal None (hint_word_count "this matches nothing" p) );
    ( "hint_word_count: returns None when the node is already solved" >:: fun _ ->
      let leaf =
        { label = "solved"; answer = "Paris"; children = []; solved = true }
      in
      let p =
        {
          id = 0;
          difficulty = "easy";
          theme = "";
          title = "";
          solved_puzzle = false;
          root =
            {
              label = "{0}";
              answer = "Paris";
              children = [ leaf ];
              solved = false;
            };
        }
      in
      assert_equal None (hint_word_count "solved" p) );

    (* ── integration ─────────────────────────────────────────────────── *)
    ( "integration: score is positive after solving all nodes of a puzzle"
    >:: fun _ ->
      let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json" in
      let p = List.hd puzzles in
      let s = ref (fresh ()) in
      let rec solve_all () =
        if not (is_won p) then begin
          List.iter
            (fun n ->
              ignore (submit n.answer p);
              s := apply_correct_from_puzzle !s p n)
            (exposed p);
          solve_all ()
        end
      in
      solve_all ();
      assert_bool "score should be positive after winning" (!s.score > 0) );
    ( "integration: a session with hints scores lower than a perfect session"
    >:: fun _ ->
      let perfect = apply_correct (fresh ()) "hard" 2 in
      let s0 = fresh () in
      let s1 = apply_hint s0 in
      let hinted = apply_correct s1 "hard" 2 in
      assert_bool "perfect should outscore hinted" (perfect.score > hinted.score)
    );
    ( "integration: a session with wrongs scores lower than a clean session"
    >:: fun _ ->
      let clean = apply_correct (fresh ()) "medium" 1 in
      let s0 = fresh () in
      let s1 = apply_wrong s0 in
      let dirty = apply_correct s1 "medium" 1 in
      assert_bool "clean should outscore dirty" (clean.score > dirty.score) );
    ( "integration: applying a time bonus after winning increases the score"
    >:: fun _ ->
      let s = apply_correct (fresh ()) "easy" 0 in
      let s_timed = apply_time_bonus s 45 in
      assert_bool "timed score should exceed base score"
        (s_timed.score > s.score) );
  ]

let _ = run_test_tt_main tests
