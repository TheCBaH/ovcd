[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5-5"]
[@@@ai_provider "Anthropic"]

(** FST input, and choosing between VCD and FST by content.

    An FST file parses to what {!Vcd.parse_file} makes of the VCD that GTKWave's [fst2vcd -e] writes for it, with the
    same identifier codes, except that reals keep every bit rather than [fst2vcd]'s 16 digits. Value changes are decoded
    one FST block at a time as the simulation sequence is consumed. {!parse_file}, {!open_file} and {!open_at} close the
    file when the sequence ends or a pull raises; {!with_file} and {!File} leave closing to the caller.

    FST times are unsigned 64-bit, but {!Vcd_types.Timestamp.compare} is signed: a time at or above 2{^ 63} sorts before
    0 in time ranges and {!Vcd.Stateful}. Such files are not supported.

    Usage:
    {[
      let { Vcd.header; simulation } = Vcd_fst.open_file "sim.fst" in
      ...
    ]} *)

type format = [ `Vcd | `Fst ]

val detect_string : string -> format
(** [`Fst] when the data starts with an FST header block or a gzip-wrapped FST, [`Vcd] otherwise. *)

val detect_file : string -> format
(** {!detect_string} on the first bytes of a file. Raises [Sys_error] if it cannot be read. *)

val parse_string : ?select:(Vcd_types.ID.t -> bool) -> string -> Vcd.parse_result
(** Parse an FST held in memory. Raises {!Vcd.Parse_error}.

    [select] is as in {!Vcd.parse_string}: the first pull calls it once per distinct ID, and the changes of unselected
    IDs are neither decompressed nor made into events. *)

val parse_file : ?select:(Vcd_types.ID.t -> bool) -> string -> Vcd.parse_result
(** Parse an FST file by path. Raises {!Vcd.Parse_error}, or [Sys_error] if it cannot be opened. *)

val open_string : ?select:(Vcd_types.ID.t -> bool) -> string -> Vcd.parse_result
(** {!parse_string} or {!Vcd.parse_string}, as {!detect_string} decides. *)

val open_file : ?select:(Vcd_types.ID.t -> bool) -> string -> Vcd.parse_result
(** {!parse_file} or {!Vcd.parse_file}, as {!detect_file} decides. *)

val with_file : ?select:(Vcd_types.ID.t -> bool) -> string -> (Vcd.parse_result -> 'a) -> 'a
(** {!Vcd.with_file} for a VCD or an FST, as {!detect_file} decides: the file is closed when [f] returns or raises. *)

val blocks : string -> (Vcd_types.Timestamp.t * Vcd_types.Timestamp.t) array
(** The start and end times of an FST file's value change blocks, read from their headers alone. Raises as {!parse_file}
    does, and [Invalid_argument] for a VCD file. *)

val open_at :
  ?select:(Vcd_types.ID.t -> bool) -> string -> time:Vcd_types.Timestamp.t -> Vcd.Stateful.state * Vcd.parse_result
(** [open_at path ~time] starts reading an FST file at a block boundary instead of at the beginning. Raises as
    {!parse_file} does, and [Invalid_argument] for a VCD file.

    The block is the last one that starts at or before [time] (the first if none does), moved back while a timestamp has
    changes both in it and in the block before. The stream therefore starts at a whole timestep, which is {e not} [time]
    in general: it is the start of that block's first timestep, and the caller skips forward.

    The state is the value of each selected ID after all changes before the stream, and the simulation continues from
    there: its events are those of the full stream from that timestep on, except that there is no [$dumpvars] section
    and no frame, and it starts with a [Timestamp]. Pass the state as [~init] to {!Vcd.Stateful.stream}, which then
    yields the full stream's events from that timestep on, [changes] alike and [state] but for the extra IDs below.

    With the first block, the state is empty and the result is that of {!parse_file}: the initial values arrive as the
    stream's [$dumpvars] changes, as usual. The state may otherwise hold IDs that the full stream has not yet assigned,
    at the value the file's frame gives them. *)

(** An open FST file, for readers that start several streams on one file (a viewer that seeks).

    The handle caches what is expensive to recompute: the header ovcd builds, the identifier codes, and what the FST
    library keeps on the file. {!parse_file} and {!open_at} open a new file for each call and decode all of that again.

    A handle is single-owner and not thread-safe. A stream {e borrows} the handle: it never closes it, at its end or on
    an error, and the caller calls {!close}. Because the streams of one handle share a file position, a new {!stream} or
    a {!close} invalidates the earlier streams: their next pull raises {!Vcd.Parse_error} ("stream superseded" or
    "handle closed"), even if events were already decoded. *)
module File : sig
  type t

  val open_ : string -> (t, string) result
  (** Open an FST file; [Error] carries the message that {!parse_file} would raise with. Raises [Invalid_argument] for a
      VCD file. *)

  val header : t -> Vcd_ast.header
  (** The header, built on first use (by this or by {!stream}) and kept. Raises {!Vcd.Parse_error} for a damaged
      hierarchy or a closed handle. *)

  val stream :
    ?select:(Vcd_types.ID.t -> bool) -> ?at:Vcd_types.Timestamp.t -> t -> Vcd.Stateful.state * Vcd_ast.event Seq.t
  (** Start a stream on the handle, invalidating the previous one. Without [at] it is that of {!parse_file}, with an
      empty state; with [at] it is that of {!open_at}, from the block that [time] falls in. The sequence is single-use
      as {!Vcd.parse_result}'s is. Raises {!Vcd.Parse_error} as {!parse_file} does. *)

  val blocks : t -> ((Vcd_types.Timestamp.t * Vcd_types.Timestamp.t) array, string) result
  (** As {!Vcd_fst.blocks}. *)

  val close : t -> unit
  (** Close the file and invalidate its streams. Idempotent. *)

  val header_builds : t -> int
  (** How many times the header was built from the hierarchy: 1 once {!header} or {!stream} has been called. *)
end
