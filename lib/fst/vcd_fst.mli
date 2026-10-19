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
