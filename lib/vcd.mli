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

module Resolver : sig
  type entry
  (** Opaque per-signal record. Use the accessors below to inspect it. *)

  val entry_id : entry -> Vcd_types.ID.t
  val entry_size : entry -> int
  val entry_reference : entry -> Vcd_types.Reference.t

  type t

  val make : Vcd_ast.header -> t
  (** Build a resolver from a parsed header. *)

  val find : t -> Vcd_types.ID.t -> entry option
  (** Look up all information for an ID. *)

  val reference : t -> Vcd_types.ID.t -> Vcd_types.Reference.t option
  (** Convenience: look up just the [Reference.t] for an ID. *)

  val find_id : t -> Vcd_types.Reference.t -> Vcd_types.ID.t option
  (** Reverse lookup: find the [ID.t] for a given [Reference.t]. *)

  val fold : (entry -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over all entries in declaration order. *)
end

type time_range = {
  start : Vcd_types.Timestamp.t option;  (** Inclusive lower bound; [None] means "from the beginning". *)
  stop : Vcd_types.Timestamp.t option;  (** Inclusive upper bound; [None] means "to the end". *)
}
(** A single half-open or closed time interval. *)

module Stateful : sig
  module State : Map.S with type key = Vcd_types.ID.t
  (** Map from [ID.t] to [Value.t], used for both full signal state and per-step changes. *)

  module ID_set : Set.S with type elt = Vcd_types.ID.t

  type state = Vcd_types.Value.t State.t

  val find : state -> Vcd_types.ID.t -> Vcd_types.Value.t option
  (** [find state id] returns the current value of [id] in [state], or [None] if unseen. *)

  type event = {
    state : state;  (** Complete tracked state as of the end of the previous timestep. *)
    time : Vcd_types.Timestamp.t;
    changes : state;
        (** IDs whose values are reported at [time]. Outside dump blocks this is the delta — only IDs that actually
            changed. At the first timestep inside each [ranges] interval this is a full snapshot of every [reported] ID
            in the current tracked state, so that the consumer sees values that changed while the stream was outside the
            range. *)
  }

  val stream : ?tracked:ID_set.t -> ?reported:ID_set.t -> ?ranges:time_range list -> Vcd_ast.event Seq.t -> event Seq.t
  (** Build a stateful sequence from a simulation event stream.

      Produces one {!event} per timestep where [event.changes] is non-empty. Signal state is always tracked regardless
      of all filters; only emission is gated.

      @param tracked IDs to maintain in [event.state]; defaults to all IDs.
      @param reported IDs to surface in [event.changes]; defaults to all IDs.
      @param ranges
        Time intervals to emit events for; defaults to all time. Multiple ranges form a union. The sequence terminates
        once the current timestamp exceeds all [stop] bounds. At the first timestep of each range interval,
        [event.changes] is a full snapshot of all [reported] IDs in the current state (not just the delta), so that
        consumers know the current value of any signal that changed while the stream was outside the range. *)
end

val string_of_scope_type : Vcd_ast.scope_type -> string
val string_of_var_type : Vcd_ast.var_type -> string
