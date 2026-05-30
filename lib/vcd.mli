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

val string_of_scope_type : Vcd_ast.scope_type -> string
val string_of_var_type : Vcd_ast.var_type -> string
