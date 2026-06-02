module ID_set = Vcd.Stateful.ID_set
(** Signal ID set; alias of [Vcd.Stateful.ID_set] so types unify when passing filter sets to [Stateful.stream]. *)

val build_filter : Vcd.Resolver.t -> (string * Filter_ast.pattern) list -> (string * Re.re) list -> ID_set.t option
(** Build a signal-ID set from DSL patterns and compiled PCRE regexes. Returns [None] when both lists are empty (meaning
    "all signals"). Emits a warning to stderr for any pattern or regex that matched nothing. *)

val build_strip_map : Vcd.Resolver.t -> ID_set.t option -> (Vcd_types.Reference.t -> Vcd_types.Reference.t) option
(** Build a function that strips the longest common scope prefix from signal references. Returns [None] when the prefix
    is empty (nothing to strip). [filter = None] means all signals contribute to the prefix computation. References not
    in the matched set are returned unchanged. *)

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
