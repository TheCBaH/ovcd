open Vcd_types

type scope_type =
  | Module
  | Task
  | Function
  | Begin
  | Fork
  | Other_scope of string
      (** Any other scope type, as written: e.g. [struct], [interface], [vhdl_architecture], [sv_array] (GTKWave). *)

type var_type =
  | Event
  | Integer
  | Parameter
  | Real
  | Realtime
  | Reg
  | Supply0
  | Supply1
  | Time
  | Tri
  | Triand
  | Trior
  | Trireg
  | Tri0
  | Tri1
  | Wand
  | Wire
  | Wor
  | Logic  (** SystemVerilog / VHDL 4-state net *)
  | Sstring  (** SystemVerilog / VHDL string variable *)
  (* SystemVerilog and GTKWave types, as written by e.g. GTKWave's fst2vcd. *)
  | Bit
  | Byte
  | Enum
  | Int
  | Longint
  | Port
  | Real_parameter
  | Shortint
  | Shortreal
  | Sparray
  | Other_type of string  (** Any other type, as written. *)

type var_decl = {
  v_type : var_type;
  size : int;
  id : Vcd_types.ID.t;
  ref : string;  (** human-readable signal name *)
  index : string option;  (** optional bit-select, e.g. "[7:0]" *)
}

type scope_node = { s_type : scope_type; s_name : string; children : scope_node list; vars : var_decl list }

type header = {
  version : string option;
  date : string option;
  timescale : string option;
  timezero : int64 option;
      (** GTKWave's [$timezero]: the offset, in timescale units, of the time that timestamps count from. The absolute
          time of a timestamp [t] is [t + timezero]. *)
  comment : string option;
  scopes : scope_node list;
}

type event =
  | Timestamp of Vcd_types.Timestamp.t
  | Change of Vcd_types.ID.t * Value.t  (** (id_code, new_value) *)
  | DumpStart of string  (** "$dumpvars" / "$dumpall" / … *)
  | DumpEnd  (** "$end" closing a dump block *)
  | SimComment of string

val var_type_of_string : string -> var_type
(** The [var_type] for a type keyword that is not one of the lexer's IEEE 1364 tokens. *)

val var_type_of_keyword : string -> var_type
(** The [var_type] for any [$var] type keyword, as the parser maps it. *)

val scope_type_of_keyword : string -> scope_type
(** The [scope_type] for any [$scope] type keyword, as the parser maps it. *)

val pack_bits : string -> int -> bytes
(** Pack a binary string (MSB-first, left-to-right) into a [bytes] value. Within each byte the MSB is bit 7. If
    [len mod 8 <> 0] the first byte is left-padded with zeros, so the bytes are the value as a big-endian number. *)

val parse_vector : string -> Value.t
(** Decode the raw binary string that follows [b]/[B] in a VCD value-change record into the richest applicable [value]
    variant: all-0/1 digits are an [Int] up to [Value.int_bits], an [Int64] up to 64, [Bytes] beyond; all IEEE 1164
    9-state digits are [Scalars]; anything else is [Other]. The per-event string length is not stored; look up
    {!var_decl.size} in the header for the signal width. *)

val binary_value : string -> Value.t
(** The digits of a [b] record as the lexer decodes them: as {!parse_vector}, through the {!Value.int} and
    {!Value.int64} constructors. *)

val packed_value : bytes -> int -> int -> Value.t
(** [packed_value b off width]: the [width] digits packed MSB first from [off] in [b] (as FST stores 0/1 vectors: the
    last byte's low bits unused), as {!binary_value} of the digits would give them. *)

val scalar_of_char : char -> Value.t
(** A scalar value change: one of the 9-state characters, or ['?'] (kept as [Other]). *)
