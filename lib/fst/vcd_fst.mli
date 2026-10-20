[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5-5"]
[@@@ai_provider "Anthropic"]

(** FST input, and choosing between VCD and FST by content.

    An FST file parses to what {!Vcd.parse_file} makes of the VCD that GTKWave's [fst2vcd -e] writes for it, with the
    same identifier codes, except that reals keep every bit rather than [fst2vcd]'s 16 digits. Value changes are decoded
    one FST block at a time as the simulation sequence is consumed; the file is closed when it ends.

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
