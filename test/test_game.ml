open OUnit2
open Cs3110_final.Types
open Cs3110_final.Parser
open Cs3110_final.Game

(** list of puzzles from the json file*)
let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json"

(** the first puzzle in the loaded puzzles*)
let puzzle = List.hd puzzles

(** a random hard puzzle in the list of loaded puzzles*)
let puzzle_hard = choose_puzzle "hard" puzzles |> Option.get

(** Pretty-print a label_part list so assert_equal failures are readable. *)
let string_of_part = function
  | Text s -> Printf.sprintf "Text %S" s
  | Slot i -> Printf.sprintf "Slot %d" i

(** A dummy child node for testing purposes. Represents a simple bracket node in
    a puzzle. *)
let dummy_node = { label = ""; answer = ""; children = []; solved = false }

let string_of_parts parts =
  "[" ^ String.concat "; " (List.map string_of_part parts) ^ "]"

let tests =
  "test suite"
  >::: [
         ( "parse_puzzle parses puzzle id correctly" >:: fun _ ->
           assert_equal 1 puzzle.id );
         ( "parse_puzzle parses title correctly" >:: fun _ ->
           assert_equal "GM's 100 Millionth Car" puzzle.title );
         ( "parse_puzzle parses difficulty correctly" >:: fun _ ->
           assert_equal "hard" puzzle.difficulty );
         ( "parse_puzzle and parse_node parses the root node and its label \
            correctly"
         >:: fun _ ->
           assert_equal "{0}{1} makes its 100 millionth car" puzzle.root.label
         );
         ( "parse_puzzle and parse_node parses the answer to prompts correctly"
         >:: fun _ ->
           assert_equal "GM makes its 100 millionth car" puzzle.root.answer );
         ( "parse_node is able to parse all of the child nodes (brackets)"
         >:: fun _ ->
           let children = puzzle.root.children in
           assert_equal 2 (List.length children);
           assert_equal "G" (List.hd children).answer;
           assert_equal "silent" (List.hd (List.hd children).children).answer );
         ( "tokenize_label handles text-slot-text-slot-text (spec example)"
         >:: fun _ ->
           assert_equal
             [
               Text "like most ";
               Slot 0;
               Text "lin and ";
               Slot 1;
               Text " movies";
             ]
             (tokenize_label "like most {0}lin and {1} movies")
             ~printer:string_of_parts );
         ( "tokenize_label emits no empty Text between adjacent slots"
         >:: fun _ ->
           assert_equal
             [ Slot 0; Slot 1; Text " makes its 100 millionth car" ]
             (tokenize_label "{0}{1} makes its 100 millionth car")
             ~printer:string_of_parts );
         ( "tokenize_label returns a single Text when no placeholders exist"
         >:: fun _ ->
           assert_equal
             [ Text "stick for dry lips" ]
             (tokenize_label "stick for dry lips")
             ~printer:string_of_parts );
         ( "tokenize_label handles a slot at the very start (no leading Text)"
         >:: fun _ ->
           assert_equal [ Slot 0; Text "lin" ] (tokenize_label "{0}lin")
             ~printer:string_of_parts );
         ( "tokenize_label parses multi-digit slot indices" >:: fun _ ->
           assert_equal
             [ Slot 10; Text "x"; Slot 2 ]
             (tokenize_label "{10}x{2}")
             ~printer:string_of_parts );
         ( "tokenize_label treats a stray '{' with no closing '}' as literal \
            text"
         >:: fun _ ->
           assert_equal
             [ Text "cost is {5 dollars" ]
             (tokenize_label "cost is {5 dollars")
             ~printer:string_of_parts );
         ( "choose_puzzle returns a random puzzle with the right difficulty \
            and None if there are no puzzles"
         >:: fun _ ->
           let difficult_puzzle = choose_puzzle "hard" puzzles in

           match difficult_puzzle with
           | None -> assert_failure "Expected Some but got None"
           | Some p ->
               assert_equal "hard" p.difficulty;

               let easy_puzzle = Option.get (choose_puzzle "easy" puzzles) in
               assert_equal "computer science" easy_puzzle.theme;
               let no_puzzle = choose_puzzle "medium" puzzles in
               assert_equal None no_puzzle );
         ( "testing normalize function" >:: fun _ ->
           let word = "    good   " in
           assert_equal "GOOD" (normalize word) );
         ( "render strips the outermost brackets from render_node s.root when \
            the root is unsolved"
         >:: fun _ ->
           let full = render_node puzzle_hard.root in
           let body = String.sub full 1 (String.length full - 2) in
           assert_equal body (render puzzle_hard) ~printer:(fun s -> s) );
         ( "render returns the root's answer verbatim when the root is solved"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           p.root.solved <- true;
           assert_equal "GM makes its 100 millionth car" (render p) );
         ( "exposed returns every leaf of a fresh puzzle (nothing solved yet)"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let answers = List.map (fun n -> n.answer) (exposed p) in
           assert_equal 6 (List.length answers);
           assert_bool "Chap should be exposed" (List.mem "Chap" answers);
           assert_bool "forward should be exposed" (List.mem "forward" answers)
         );
         ( "exposed drops a solved leaf but keeps its siblings" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let _ = submit "Chap" p in
           let answers = List.map (fun n -> n.answer) (exposed p) in
           assert_bool "Chap should no longer be exposed"
             (not (List.mem "Chap" answers));
           assert_bool "name should still be exposed" (List.mem "name" answers)
         );
         ( "choose_puzzle only returns unsolved puzzles" >:: fun _ ->
           let p1 =
             {
               id = 1;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = true;
               root = dummy_node;
             }
           in
           let p2 =
             {
               id = 2;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in

           let puzzles = [ p1; p2 ] in
           assert_equal (Some p2) (choose_puzzle "easy" puzzles);
           let puzzles =
             [
               {
                 id = 1;
                 difficulty = "easy";
                 theme = "";
                 title = "";
                 solved_puzzle = true;
                 root = dummy_node;
               };
               {
                 id = 2;
                 difficulty = "easy";
                 theme = "";
                 title = "";
                 solved_puzzle = false;
                 root = dummy_node;
               };
               {
                 id = 3;
                 difficulty = "easy";
                 theme = "";
                 title = "";
                 solved_puzzle = false;
                 root = dummy_node;
               };
             ]
           in
           let puzzle = choose_puzzle "easy" puzzles in
           let is_correct =
             match puzzle with
             | Some p -> p.id = 2 || p.id = 3
             | None -> false
           in
           assert_equal true is_correct );
         ( " choose_puzzle only returns unsolved puzzles of the right difficulty"
         >:: fun _ ->
           let p1 =
             {
               id = 1;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in
           let p2 =
             {
               id = 2;
               difficulty = "medium";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in
           let p3 =
             {
               id = 3;
               difficulty = "hard";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in

           let puzzles = [ p1; p2; p3 ] in
           assert_equal (Some p1) (choose_puzzle "easy" puzzles) );
       ]

let _ = run_test_tt_main tests
