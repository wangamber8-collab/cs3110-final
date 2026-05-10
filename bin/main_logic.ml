(* =============================================================================
   main_logic.ml — Dynamic game loop for Bracket City
   =============================================================================
   This file is the replacement for main.ml once the game is fully wired up. It
   connects the OCaml game engine (lib/game.ml, lib/parser.ml) to the Dream
   WebSocket server so that the browser frontend gets real puzzle data instead
   of the hardcoded strings in main.ml.

   TO RUN THIS SERVER (instead of main.ml): dune exec bin/main_logic.exe from
   the project root directory.

   ARCHITECTURE OVERVIEW: - All puzzles are loaded from JSON once at startup
   into [all_puzzles]. - Each WebSocket connection (one per browser tab) gets
   its own puzzle [ref], so players are independent. This is "per-session" mode.
   - The game state IS the puzzle tree — Game.submit mutates [node.solved] flags
   in place, so [!state] always reflects current progress without any extra
   bookkeeping. - The frontend (game-scripts.js) already handles "BRACKET|..."
   messages. New message types (INCORRECT, WIN, EXPOSED) are stubbed out below
   and just need matching JS handlers to be activated.
   ============================================================================= *)

open Lwt.Syntax
open Cs3110_final

(* -----------------------------------------------------------------------------
   STARTUP: Load all puzzles from the JSON data file.
   -----------------------------------------------------------------------------
   This runs exactly once when the module is first loaded (i.e. at server
   startup), not on every request. All WebSocket sessions share this list and
   call choose_puzzle to get their own independent copy.

   PATH NOTE: "data/ver2_NESTED_puzzles.json" is relative to the directory where
   you run `dune exec`, which is the project root. This differs from the test
   suite, which uses "../data/..." because tests run from _build/default/test/.
   ----------------------------------------------------------------------------- *)
let all_puzzles : Types.puzzle list =
  Parser.load_puzzles "data/ver2_NESTED_puzzles.json"

(* let default_difficulty : string = "hard" *)

(* -----------------------------------------------------------------------------
   CONNECTED CLIENTS LIST
   -----------------------------------------------------------------------------
   [clients] tracks every open WebSocket. It is only used by [_broadcast_all],
   which is stubbed out for a future collaborative mode. In per-session mode
   (the current design) we never need to contact all clients — each handler only
   talks to its own [ws].

   If you remove collaborative mode entirely, you can delete this ref and
   [_broadcast_all] without touching anything else.
   ----------------------------------------------------------------------------- *)
let clients : Dream.websocket list ref = ref []

(* Remove a single WebSocket from the global client list on disconnect. *)
let remove_client (ws : Dream.websocket) : unit =
  clients := List.filter (fun c -> c != ws) !clients

(* -----------------------------------------------------------------------------
   send_to: Send one message to one WebSocket, silently ignoring errors.
   -----------------------------------------------------------------------------
   Dream.send raises an exception if the socket is already closed (e.g. the
   browser tab was closed mid-game). We catch that here so the server does not
   crash on a disconnected client.
   ----------------------------------------------------------------------------- *)
let send_to (ws : Dream.websocket) (msg : string) : unit Lwt.t =
  Lwt.catch (fun () -> Dream.send ws msg) (fun _exn -> Lwt.return_unit)

(* -----------------------------------------------------------------------------
   STUB: _broadcast_all — Send the same message to every connected client.
   -----------------------------------------------------------------------------
   Not used in per-session mode. Activate this for a collaborative mode where
   all players share one puzzle: replace [send_to ws] calls with
   [_broadcast_all] wherever you want all clients to see an update.
   ----------------------------------------------------------------------------- *)
let _broadcast_all (msg : string) : unit Lwt.t =
  Lwt_list.iter_p (fun ws -> send_to ws msg) !clients

(* -----------------------------------------------------------------------------
   send_bracket: Render current puzzle state and push it to one client.
   -----------------------------------------------------------------------------
   "BRACKET|" is the message prefix that game-scripts.js already handles:

   ws.onmessage = (event) => { if (msg.startsWith("BRACKET|")) {
   bracket1.textContent = "[" + msg.slice("BRACKET|".length) + "]"; } };

   Game.render returns the puzzle string with unsolved nodes shown as [clue] and
   solved nodes shown as their bare answer. The frontend wraps the whole thing
   in [ ] itself, so we do NOT add outer brackets here.
   ----------------------------------------------------------------------------- *)
let send_bracket (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  send_to ws ("BRACKET|" ^ Game.render !state)

let send_progress (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  let solved, total = Game.progress !state in
  send_to ws (Printf.sprintf "PROGRESS|%d|%d" solved total)

let elapsed_seconds (start_time : float) : int =
  int_of_float (Unix.gettimeofday () -. start_time)

let send_timer (ws : Dream.websocket) (start_time : float) : unit Lwt.t =
  send_to ws (Printf.sprintf "TIMER|%d" (elapsed_seconds start_time))

(* -----------------------------------------------------------------------------
   STUB: _send_exposed — Tell the client which answers are currently guessable.
   -----------------------------------------------------------------------------
   Game.exposed returns the "frontier" nodes: unsolved nodes whose children are
   all solved. At game start these are the leaves. As the player guesses
   correctly, their parents become the new frontier.

   Sending this list lets the frontend display a hint panel like: "You can
   guess: Chap, name, Michael, should, head, forward" without revealing the full
   answer tree structure.

   TO ACTIVATE: 1. Uncomment the [send_to] call in this function body. 2. Add a
   handler in game-scripts.js: if (msg.startsWith("EXPOSED|")) { const answers =
   msg.slice("EXPOSED|".length).split(","); // render answers somewhere in the
   UI as hints } 3. Uncomment the [_send_exposed] calls inside [handle_guess]
   and [ws_handler].
   ----------------------------------------------------------------------------- *)
let _send_exposed (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  let exposed_nodes = Game.exposed !state in
  let answer_list = List.map (fun (n : Types.node) -> n.answer) exposed_nodes in
  let csv = String.concat "," answer_list in
  (* STUB: uncomment the line below once the frontend handles "EXPOSED|" *)
  (* send_to ws ("EXPOSED|" ^ csv) *)
  ignore csv;
  ignore ws;
  Lwt.return_unit

(* -----------------------------------------------------------------------------
   STUB: _send_incorrect — Tell the client their guess was wrong.
   -----------------------------------------------------------------------------
   Currently a wrong guess causes nothing visible to happen — the input clears
   and the bracket stays the same, which is confusing. This stub sends the
   rejected guess back so the frontend can flash an error or shake the input.

   TO ACTIVATE: 1. Uncomment the [send_to] call in this function body. 2. Add a
   handler in game-scripts.js: if (msg.startsWith("INCORRECT|")) { const
   badGuess = msg.slice("INCORRECT|".length); // flash error, shake input box,
   increment wrong-guess counter, etc. } 3. Uncomment the [_send_incorrect] call
   inside [handle_guess].
   ----------------------------------------------------------------------------- *)
let _send_incorrect (ws : Dream.websocket) (guess : string) : unit Lwt.t =
  (* STUB: uncomment the line below once the frontend handles "INCORRECT|" *)
  send_to ws ("INCORRECT|" ^ guess)

(* -----------------------------------------------------------------------------
   send_stats: Send the player's victory statistics to the frontend.

   [total_guesses] counts every submitted answer attempt. [wrong_guesses] counts
   guesses that did not match any exposed node. [hints_used] counts hint/reveal
   actions.

   The number of correct guesses is computed as: total_guesses - wrong_guesses

   Accuracy is computed as an integer percentage: correct_guesses * 100 /
   total_guesses

   The message format sent to the frontend is:
   STATS|total_guesses|wrong_guesses|hints_used|accuracy
   ----------------------------------------------------------------------------- *)

let send_stats (ws : Dream.websocket) (session : Score.session ref) : unit Lwt.t
    =
  let s = !session in
  let total = Score.total_attempts s in
  let accuracy_pct = if total = 0 then 100 else s.correct_count * 100 / total in
  let sum = Score.make_summary s in
  send_to ws
    (Printf.sprintf "STATS|%d|%d|%d|%d|%d|%s|%d" total s.wrong_count
       s.hint_count accuracy_pct sum.final_score sum.grade s.max_streak)

(* -----------------------------------------------------------------------------
   STUB: _send_win — Tell the client they have solved the whole puzzle.
   -----------------------------------------------------------------------------
   Game.is_won checks whether the root node is solved. The root can only be
   solved after every node in the tree has been guessed correctly, so this fires
   exactly once per game.

   TO ACTIVATE: 1. Uncomment the [send_to] call in this function body. 2. Add a
   handler in game-scripts.js: if (msg.startsWith("WIN|")) { const finalAnswer =
   msg.slice("WIN|".length); // show congratulations screen, confetti, play
   sound, etc. } 3. Uncomment the [_send_win] call inside [handle_guess].
   ----------------------------------------------------------------------------- *)
let _send_win (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t =
  send_to ws ("WIN|" ^ !state.root.answer)

(* -----------------------------------------------------------------------------
   handle_guess: Process one guess from the player.
   -----------------------------------------------------------------------------
   This is the core of the game loop for a single turn.

   HOW Game.submit WORKS (so you understand what happens here): - Game.submit
   normalizes the guess (trim + uppercase) before comparing, so "chap", " Chap
   ", and "CHAP" all correctly match the answer "Chap". - It scans the exposed
   frontier for a node whose normalized answer matches. - If found: flips that
   node's [solved] flag to true IN PLACE inside the puzzle tree, and returns
   true. - If not found: returns false and leaves the tree unchanged.

   CORRECT GUESS FLOW: 1. Re-render the bracket and push it to the client.
   Because [solved] flags were already mutated by Game.submit, Game.render now
   shows the newly solved node as its bare answer (no brackets) and everything
   else unchanged. 2. Check Game.is_won — if the root is now solved, the whole
   puzzle is done. Send the WIN stub and stop. Otherwise send the EXPOSED stub
   so the client knows what to guess next.

   INCORRECT GUESS FLOW: Send the INCORRECT stub. The bracket display does not
   change.
   ----------------------------------------------------------------------------- *)
let handle_guess (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (start_time : float) (guess : string) :
    unit Lwt.t =
  let exposed_before = Game.exposed !state in
  let normed = Game.normalize guess in
  let maybe_node =
    List.find_opt
      (fun (n : Types.node) -> Game.normalize n.answer = normed)
      exposed_before
  in
  let correct = Game.submit guess !state in
  if correct then begin
    (session :=
       match maybe_node with
       | Some n -> Score.apply_correct_from_puzzle !session !state n
       | None -> Score.apply_correct !session !state.difficulty 0);
    let* () = send_bracket ws state in
    let* () = send_progress ws state in
    let exposed = Game.exposed !state in
    match exposed with
    | [ n ] when n == !state.root ->
        n.solved <- true;
        let* () = send_bracket ws state in
        let* () = send_progress ws state in
        let* () = send_timer ws start_time in
        let* () = send_stats ws session in
        _send_win ws state
    | _ ->
        if Game.is_won !state then begin
          let* () = send_timer ws start_time in
          let* () = send_stats ws session in
          _send_win ws state
        end
        else _send_exposed ws state
  end
  else begin
    session := Score.apply_wrong !session;
    _send_incorrect ws guess
  end

(* -----------------------------------------------------------------------------
   ws_handler: Handle one WebSocket connection (one browser session).
   -----------------------------------------------------------------------------
   Dream calls this function whenever a browser opens ws://localhost:8080/ws.

   STEP-BY-STEP: 1. Register the socket in [clients] for potential future
   broadcast_all use. 2. Pick a fresh puzzle via Parser.choose_puzzle using
   [default_difficulty]. Each call to choose_puzzle uses Random.self_init so
   sessions get different puzzles (or the same one by chance if the pool is
   small). 3. If no puzzle matches the difficulty (shouldn't happen with valid
   data), log the error, send an ERROR| message, and close the socket cleanly.
   4. Wrap the puzzle in a [ref] so [handle_guess] can read its [solved] flags
   across the lifetime of the keep_open loop. 5. Send the initial BRACKET| so
   the player sees the puzzle immediately on connect, before typing anything. 6.
   Enter [keep_open]: a tail-recursive Lwt loop that blocks on Dream.receive,
   processes each incoming guess via [handle_guess], and exits when the client
   disconnects (Dream.receive returns None). 7. [Lwt.finalize] guarantees
   [remove_client] runs whether the loop exits cleanly or an exception is
   raised.

   STUB NOTE — difficulty from the browser: To let the player choose difficulty
   on a lobby page, change game-scripts.js to open the WebSocket with a query
   param: const ws = new WebSocket(`ws://${location.host}/ws?difficulty=easy`);
   Then replace [ignore req] and [default_difficulty] here with: let diff =
   Dream.query req "difficulty" |> Option.value ~default:"hard" in
   ----------------------------------------------------------------------------- *)
let handle_hint (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (chip_body : string) : unit Lwt.t =
  match Game.hint_first_letter chip_body !state with
  | None -> Lwt.return_unit
  | Some letter ->
      session := Score.apply_hint !session;
      send_to ws ("HINT|" ^ letter)

let handle_reveal (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (start_time : float) (chip_body : string) :
    unit Lwt.t =
  if Game.reveal_by_body chip_body !state then begin
    session := Score.apply_hint !session;
    let* () = send_bracket ws state in
    let* () = send_progress ws state in

    let exposed = Game.exposed !state in
    match exposed with
    | [ n ] when n == !state.root ->
        n.solved <- true;
        let* () = send_bracket ws state in
        let* () = send_progress ws state in
        let* () = send_timer ws start_time in
        let* () = send_stats ws session in
        _send_win ws state
    | _ ->
        if Game.is_won !state then begin
          let* () = send_timer ws start_time in
          let* () = send_stats ws session in
          _send_win ws state
        end
        else Lwt.return_unit
  end
  else Lwt.return_unit

let ws_handler (req : Dream.request) : Dream.response Lwt.t =
  let diff = Dream.query req "difficulty" |> Option.value ~default:"hard" in
  Dream.log "Chosen difficulty from query: %s" diff;
  Dream.websocket (fun ws ->
      clients := ws :: !clients;

      let state_opt = Parser.choose_puzzle diff all_puzzles in

      (* Lwt.finalize ensures cleanup runs even if an exception bubbles up. *)
      Lwt.finalize
        (fun () ->
          match state_opt with
          | None ->
              Dream.log "No puzzles found for difficulty: %s" diff;
              send_to ws ("ERROR|No puzzles available for difficulty: " ^ diff)
          | Some puzzle ->
              let state = ref puzzle in
              let session = ref (Score.make_session ()) in
              let start_time = Unix.gettimeofday () in
              (* Push the initial render so the player sees the puzzle on
                 load. *)
              let* () = send_bracket ws state in
              let* () = send_progress ws state in
              let* () = send_timer ws start_time in

              (* STUB: also push the initial exposed set once frontend handles it.
                 Uncomment the line below when _send_exposed is activated. *)
              (* let* () = _send_exposed ws state in *)

              (* Process guesses one at a time until the client disconnects. *)
              let rec keep_open () =
                let* msg = Dream.receive ws in
                match msg with
                | None ->
                    (* None = client closed the tab or lost connection. *)
                    Lwt.return_unit
                | Some raw ->
                    let* () =
                      if String.length raw >= 5 && String.sub raw 0 5 = "HINT|"
                      then
                        handle_hint ws state session
                          (String.sub raw 5 (String.length raw - 5))
                      else if
                        String.length raw >= 7 && String.sub raw 0 7 = "REVEAL|"
                      then
                        handle_reveal ws state session start_time
                          (String.sub raw 7 (String.length raw - 7))
                      else handle_guess ws state session start_time raw
                    in
                    keep_open ()
              in
              keep_open ())
        (fun () ->
          remove_client ws;
          Lwt.return_unit))

(* -----------------------------------------------------------------------------
   ENTRY POINT: Start the Dream HTTP + WebSocket server.
   -----------------------------------------------------------------------------
   Routes are identical to main.ml so the same frontend files are served. The
   only change is /ws now calls our dynamic [ws_handler] instead of the
   hardcoded one in main.ml.

   HOW TO SWITCH THE PROJECT TO THIS FILE: 1. Rename bin/main.ml →
   bin/main_stub.ml (keep it for reference) 2. Rename bin/main_logic.ml →
   bin/main.ml 3. Revert bin/dune back to: (executable (name main) (libraries
   dream yojson cs3110_final))
   ----------------------------------------------------------------------------- *)
let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (Dream.from_filesystem "public" "index.html");
         Dream.get "/game" (Dream.from_filesystem "public" "game.html");
         Dream.get "/ws" ws_handler;
         Dream.get "/**" (Dream.static "public");
       ]

(* debug log*)
(* let handle_guess (ws : Dream.websocket) (state : Types.puzzle ref)
    (guess : string) : unit Lwt.t =
  let correct = Game.submit guess !state in
  if correct then begin
    let* () = send_bracket ws state in
    let* () = send_progress ws state in
    let won = Game.is_won !state in
    Dream.log "is_won = %b" won;
    if won then _send_win ws state else _send_exposed ws state
  end
  else _send_incorrect ws guess *)
