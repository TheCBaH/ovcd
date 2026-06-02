module ID_set = Vcd.Stateful.ID_set
(** Signal ID set; alias of [Vcd.Stateful.ID_set] so types unify when passing filter sets to [Stateful.stream]. *)

type filter
(** Maps each matched [ID.t] to the specific subset of its [Reference.t] aliases that were selected by the user's
    patterns. Keeping per-alias granularity avoids printing unselected aliases of a matched ID. *)

val build_filter : Vcd.Resolver.t -> (string * Filter_ast.pattern) list -> (string * Re.re) list -> filter option
(** Build a filter from DSL patterns and compiled PCRE regexes. Returns [None] when both lists are empty (meaning "all
    signals"). Emits a warning to stderr for any pattern or regex that matched nothing. *)

val filter_ids : filter -> ID_set.t
(** Extract the set of matched IDs, for use with [Stateful.stream ~reported] or [Stateful.stream ~tracked]. *)

val mem_filter : filter option -> Vcd_types.ID.t -> bool
(** [true] when [filter = None] (all signals pass) or the ID is present in the filter. *)

val selected_refs : Vcd.Resolver.t -> filter option -> Vcd_types.ID.t -> Vcd.Resolver.Ref_set.t
(** The references to print for [id] given [filter]. [None] → all references from the resolver. [Some f] → only the
    subset of references that were explicitly selected for this ID. *)

val build_strip_map : Vcd.Resolver.t -> filter option -> (Vcd_types.Reference.t -> Vcd_types.Reference.t) option
(** Build a function that strips the longest common scope prefix from the selected signal references. Returns [None]
    when the prefix is empty (nothing to strip). [filter = None] means all signals contribute to the prefix computation.
    References not in the selected set are returned unchanged by the returned function. *)

val parse_range : string -> Vcd.time_range
(** Parse a single time-range specification string into a {!Vcd.time_range}.

    Syntax:
    - ["...N"] or ["..N"] or ["-N"] — from the start to [N] (inclusive)
    - ["N-..."] or ["N-.."] or ["N-"] — from [N] to the end
    - ["N-M"] — from [N] to [M] (both inclusive)
    - ["N"] — point range [[N, N]]
    - ["..."] or [".."] — all time

    Raises [Failure] on invalid input. *)

val in_ranges : Vcd.time_range list -> Vcd_types.Timestamp.t -> bool
(** [in_ranges ranges t] is [true] when [t] falls inside at least one range, or when [ranges] is empty (all time). *)

val past_all_ranges : Vcd.time_range list -> Vcd_types.Timestamp.t -> bool
(** [past_all_ranges ranges t] is [true] when [t] is strictly past every range's [stop]. Early-exit guard. Always
    [false] when [ranges] is empty or any range has no [stop]. *)
