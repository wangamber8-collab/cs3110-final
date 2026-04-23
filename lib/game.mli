(** Game logic for the nested-bracket puzzle. The parsed puzzle is the game
    state — [solved] flags on nodes are flipped as the player wins.

    NOTE: several values below ([normalize], [tokenize_label], [render_node])
    are technically internal helpers. They are exposed here so the test suite
    can cover them directly. *)

(** The current game state. Alias of [Types.puzzle] — the parsed puzzle tree
    already carries a [solved] flag set to [false] on every node. *)
type state = Types.puzzle

(** [normalize s] returns [s] trimmed and lowercased so that user input can be
    compared to a node's answer regardless of case or surrounding whitespace.
    Internal whitespace is preserved (e.g. "Apollo 11" stays "apollo 11"). *)
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
    nodes whose children are all solved. Initially this is the set of leaves;
    as the player wins nodes, their parents become exposed. *)
val exposed : state -> Types.node list

(** [submit input s] tries [input] as an answer. If it matches the answer of
    some exposed node (after [normalize]), returns the updated state with
    that node marked solved and [true]; otherwise returns [(s, false)]. *)
val submit : string -> state -> state * bool

(** [is_won s] is [true] once the root node has been solved — equivalently,
    once every node in the tree has been solved. *)
val is_won : state -> bool
