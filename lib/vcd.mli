[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-sonnet-4-6"]
[@@@ai_provider "Anthropic"]

(** High-level API for the VCD parser.

    Usage:
    {[
      let ic = open_in "sim.vcd" in
      let { Vcd_ast.header; _ } = Vcd.parse_channel ic in
      ...
    ]} *)

exception Parse_error of string
(** Raised with a human-readable message on parse errors. *)

type parse_result = {
  header : Vcd_ast.header;  (** Parsed header / declaration section. *)
  simulation : Vcd_ast.event Seq.t;  (** Lazy sequence of simulation events; evaluated on demand. *)
}

val parse_string : string -> parse_result
(** Parse a VCD document from an in-memory string. *)

val parse_channel : in_channel -> parse_result
(** Parse from an already-open [in_channel]. *)

val parse_file : string -> parse_result
(** Parse a VCD file by path. The simulation [Seq.t] is lazy; the file handle is left open until the sequence is fully
    consumed or the caller discards it. *)

module ID_map : Map.S with type key = Vcd_types.ID.t
(** Map keyed on signal identifiers. *)

module ID_set : Set.S with type elt = Vcd_types.ID.t
(** Set of signal identifiers. *)

module Ref_map : Map.S with type key = Vcd_types.Reference.t
(** Map keyed on hierarchical signal references. *)

module Ref_set : Set.S with type elt = Vcd_types.Reference.t
(** Set of hierarchical signal references. *)

module Resolver : sig
  type entry
  (** Opaque per-signal record. One entry per unique ID; may carry multiple [Reference.t] aliases. *)

  val entry_id : entry -> Vcd_types.ID.t
  val entry_size : entry -> int

  val entry_references : entry -> Ref_set.t
  (** All hierarchical names that map to this ID (at least one; more when the signal is aliased). *)

  type t

  val make : Vcd_ast.header -> t
  (** Build a resolver from a parsed header. *)

  val find : t -> Vcd_types.ID.t -> entry option
  (** Look up all information for an ID. *)

  val references : t -> Vcd_types.ID.t -> Ref_set.t
  (** Convenience: return all [Reference.t] values for an ID; empty set if the ID is unknown. *)

  val find_id : t -> Vcd_types.Reference.t -> Vcd_types.ID.t option
  (** Reverse lookup: find the [ID.t] for a given [Reference.t]. *)

  val fold : (entry -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over all entries in declaration order. *)

  val find_all : Filter_ast.pattern -> t -> entry list
  (** [find_all pat t] returns all entries that have at least one reference matched by [pat]. Order is unspecified. *)
end

type time_range = {
  start : Vcd_types.Timestamp.t option;  (** Inclusive lower bound; [None] means "from the beginning". *)
  stop : Vcd_types.Timestamp.t option;  (** Inclusive upper bound; [None] means "to the end". *)
}
(** A single half-open or closed time interval. *)

val in_ranges : time_range list -> Vcd_types.Timestamp.t -> bool
(** [in_ranges ranges t] is [true] when [t] falls inside at least one range, or when [ranges] is empty (all time). *)

val past_all_ranges : time_range list -> Vcd_types.Timestamp.t -> bool
(** [past_all_ranges ranges t] is [true] when [t] is strictly past every range's [stop]. Always [false] when [ranges] is
    empty or any range has no [stop]. *)

module Stateful : sig
  type state = Vcd_types.Value.t ID_map.t
  (** Map from [ID.t] to [Value.t], used for both full signal state and per-step changes. *)

  val find : state -> Vcd_types.ID.t -> Vcd_types.Value.t option
  (** [find state id] returns the current value of [id] in [state], or [None] if unseen. *)

  type event = {
    state : state;  (** Current values of the effective ID set, as of the end of the previous timestep. *)
    time : Vcd_types.Timestamp.t;
    changes : state;
        (** IDs in the effective set whose values changed at [time]. Outside dump blocks this is the delta. At the first
            timestep inside each [ranges] interval this is a full snapshot of the effective ID set in the current state,
            so that the consumer sees values that changed while the stream was outside the range. *)
  }

  val stream : ?tracked:ID_set.t -> ?reported:ID_set.t -> ?ranges:time_range list -> Vcd_ast.event Seq.t -> event Seq.t
  (** Build a stateful sequence from a simulation event stream.

      The {e effective ID set} is [tracked ∪ reported] when both are given, or whichever is given when only one is, or
      all IDs when neither is. Both [event.state] and [event.changes] are restricted to this set.

      Produces one {!event} per timestep where [event.changes] is non-empty.

      @param tracked IDs to include in the effective set.
      @param reported Additional IDs to include in the effective set.
      @param ranges
        Time intervals to emit events for; defaults to all time. Multiple ranges form a union. The sequence terminates
        once the current timestamp exceeds all [stop] bounds. At the first timestep of each range interval,
        [event.changes] is a full snapshot of the effective set in the current state (not just the delta), so that
        consumers know the current value of any signal that changed while the stream was outside the range. *)
end

val string_of_scope_type : Vcd_ast.scope_type -> string
val string_of_var_type : Vcd_ast.var_type -> string
