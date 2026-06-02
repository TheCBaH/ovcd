module ID_set : Set.S with type elt = Vcd_types.ID.t
(** Signal ID set. *)

val build_filter : Vcd.Resolver.t -> (string * Filter_ast.pattern) list -> (string * Re.re) list -> ID_set.t option
(** Build a signal-ID set from DSL patterns and compiled PCRE regexes. Returns [None] when both lists are empty (meaning
    "all signals"). Emits a warning to stderr for any pattern or regex that matched nothing. *)

val build_strip_map : Vcd.Resolver.t -> ID_set.t option -> (Vcd_types.Reference.t -> Vcd_types.Reference.t) option
(** Build a function that strips the longest common scope prefix from signal references. Returns [None] when the prefix
    is empty (nothing to strip). [filter = None] means all signals contribute to the prefix computation. References not
    in the matched set are returned unchanged. *)
