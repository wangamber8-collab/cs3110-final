(** Game logic for the nested-bracket puzzle. The parsed puzzle is the game
    state — [solved] flags on nodes are flipped as the player wins.

    NOTE: several values below ([normalize], [tokenize_label], [render_node])
    are technically internal helpers. They are exposed here so the test suite
    can cover them directly. *)

(** The current game state. Alias of [Types.puzzle] — the parsed puzzle tree
    already carries a [solved] flag set to [false] on every node. *)
type state = Types.puzzle

(** [normalize s] returns [s] trimmed and uppercased so that user input can be
    compared to a node's answer regardless of case or surrounding whitespace.
    Internal whitespace is preserved (e.g. "Apollo 11" stays "APOLLO 11"). *)
val normalize : string -> string

(** [tokenize_label label] splits a label on [{N}] placeholders, returning an
    alternating list of literal [Text] segments and [Slot] indices. A stray
    ['{'] with no matching digits+['}'] is kept as literal text. *)
val tokenize_label : string -> Types.label_part list

(** [render_node n] renders a single node to its display string. A solved node
    shows its answer verbatim; an unsolved node shows its label with slots
    recursively filled by its children, wrapped in ["[" ... "]"]. *)
val render_node : Types.node -> string

(** [render s] is the full puzzle as shown to the player. Same as
    [render_node s.root] but with the outermost brackets stripped. *)
val render : state -> string

(** [exposed s] returns every node the player can currently attempt: unsolved
    nodes whose children are all solved. Initially this is the set of leaves; as
    the player wins nodes, their parents become exposed. *)
val exposed : state -> Types.node list

(** [submit input s] tries [input] as an answer. If it matches the answer of
    some exposed node (after [normalize]), the state is modified where the node
    is marked solved, returning [true]; otherwise returns [false]. *)
val submit : string -> state -> bool

(** [is_won s] is [true] once the root node has been solved — equivalently, once
    every node in the tree has been solved. *)
val is_won : state -> bool

(** [count_nodes n] returns the total number of nodes in the subtree rooted at
    [n], counting both solved and unsolved nodes. NOTE: exposed for testing only
    — prefer [progress] in application code. *)
val count_nodes : Types.node -> int

(** [count_solved n] returns the number of solved nodes in the subtree rooted at
    [n]. NOTE: exposed for testing only — prefer [progress] in application code.
*)
val count_solved : Types.node -> int

(** [progress s] returns [(solved, total)] where [solved] is the number of nodes
    the player has correctly answered and [total] is the total node count. Use
    this to drive a progress bar or "X of Y" display. *)
val progress : state -> int * int

(** [hint_first_letter chip_body s] finds the exposed node whose rendered body
    matches [chip_body] and returns [Some c] where [c] is the first character of
    that node's answer. Returns [None] if no exposed node matches or the answer
    is empty. [chip_body] is the text inside the clicked bracket chip, i.e. the
    rendered label with child answers substituted but without the outer
    ["[" ... "]"]. *)
val hint_first_letter : string -> state -> string option

(** [reveal_by_body chip_body s] finds the exposed node whose rendered body
    matches [chip_body] and marks that node as solved. Returns [true] if a
    matching exposed node was found and revealed, and [false] otherwise.
    [chip_body] is the text inside the clicked bracket chip, i.e. the rendered
    label with child answers substituted but without the outer ["[" ... "]"].

    This is used when the player clicks a bracket a second time: the first click
    gives a hint, and the second click reveals/solves that currently answerable
    bracket. *)
val reveal_by_body : string -> state -> bool

(** [hint_answer_length chip_body s] finds the exposed node whose rendered
    body matches [chip_body] and returns [Some n] where [n] is the character
    count of that node's answer. Returns [None] if no exposed node matches or
    the answer is empty. *)
val hint_answer_length : string -> state -> int option

(** [hint_word_count chip_body s] finds the exposed node whose rendered body
    matches [chip_body] and returns [Some n] where [n] is the number of
    space-separated words in that node's answer. Returns [None] if no exposed
    node matches or the answer is empty. *)
val hint_word_count : string -> state -> int option
