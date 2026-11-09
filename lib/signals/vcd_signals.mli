(** Signal declarations of a VCD or FST header as a flat sequence, and a JSON Lines codec for them.

    The lines are what [vcd_dump --list-signals PATTERN --format jsonl] prints, so a script or another OCaml program can
    read them with {!read} or {!of_line}. *)

type t = {
  path : string;  (** Hierarchical name joined with ['.'], as [vcd_dump] prints it. *)
  parts : string list;  (** The same name by component, outermost scope first; exact when a name contains ['.']. *)
  scope_types : string list;  (** Kind of each enclosing scope ([module], [begin], ...), outermost first. *)
  id : string;  (** The ID code. Aliases of one signal share it. *)
  width : int;
  var_type : string;  (** [wire], [reg], [integer], ..., as [vcd_dump] prints it. *)
  index : string option;  (** The bit-select written after the name, e.g. [[7:0]]. *)
}

val of_header : Vcd_ast.header -> t Seq.t
(** One element per [$var], aliases included. The variables of a scope come before its child scopes; the header tree
    does not keep their interleaving. *)

val jsont : t Jsont.t
(** The JSON object codec: an encoder and a decoder. [index] is left out when absent. *)

val to_line : t -> string
(** Minified JSON, without the newline. *)

val of_line : string -> (t, string) result

val write : out_channel -> t Seq.t -> unit
(** One {!to_line} per signal, each followed by a newline. *)

val read : in_channel -> (t, string) result Seq.t
(** Lazily decodes the lines of a channel, skipping empty ones. A malformed line is an [Error] element and does not end
    the sequence. *)
