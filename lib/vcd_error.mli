(** Typed errors of the VCD and FST readers, exported as {!Vcd.Error}.

    Every failure of the library's entry points is an {!E} (or an [Error _] of the [*_result] variants); none escapes as
    [Failure], [Sys_error], [Invalid_argument] or a parser exception. *)

type pos = { file : string; line : int; col : int }
(** A source position: [file] is the path, ["<string>"] or ["<channel>"]; [line] is 1-based and [col] 0-based. *)

type error =
  [ `Syntax of pos * string  (** The lexer or the header grammar rejected the text. *)
  | `Incomplete of pos * string
    (** Strict parsing only: no [$enddefinitions], a dump block open at the end, or no final newline. *)
  | `Fst of string * Fst.Error.t  (** A damaged FST file, named by path; the kind and offset are kept. *)
  | `Io of string  (** The file cannot be opened or read; the [Sys_error] message. *)
  | `Not_fst of string  (** An FST-only function was given a VCD file, named by path. *)
  | `Closed of string  (** A pull from a stream of a closed [Vcd_fst.File] handle, named by path. *)
  | `Superseded of string
    (** A pull from a stream that a later [Vcd_fst.File.stream] or [Vcd_fst.File.close] invalidated. *) ]

val pp_error : Format.formatter -> error -> unit
(** One line, without a trailing newline: for a positioned error, ["<message> at <file> line <n> col <c>"]. *)

val to_string : error -> string

exception E of error
(** Raised by every entry point, and by pulling the lazy simulation. Printed by [Printexc] through {!pp_error}. *)

val catch : (unit -> 'a) -> ('a, error) result
(** [catch f] is [Ok (f ())], or [Error e] when [f] raises [E e]. *)

(**/**)

(* Internal: raised by the lexer and grammar, turned into [E (`Syntax _)] by {!Vcd}. *)

exception Located of pos * string

val located : Lexing.position -> string -> 'a
val pos_of_lexing : Lexing.position -> pos
