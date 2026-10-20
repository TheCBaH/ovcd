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

(** The [var_type] for a type keyword that is not one of the lexer's IEEE 1364 tokens. *)
let var_type_of_string = function
  | "bit" -> Bit
  | "byte" -> Byte
  | "enum" -> Enum
  | "int" -> Int
  | "longint" -> Longint
  | "port" -> Port
  | "real_parameter" -> Real_parameter
  | "shortint" -> Shortint
  | "shortreal" -> Shortreal
  | "sparray" -> Sparray
  | s -> Other_type s

(** The [var_type] for any [$var] type keyword, as the parser maps it. *)
let var_type_of_keyword = function
  | "event" -> Event
  | "integer" -> Integer
  | "parameter" -> Parameter
  | "real" -> Real
  | "realtime" -> Realtime
  | "reg" -> Reg
  | "supply0" -> Supply0
  | "supply1" -> Supply1
  | "time" -> Time
  | "tri" -> Tri
  | "triand" -> Triand
  | "trior" -> Trior
  | "trireg" -> Trireg
  | "tri0" -> Tri0
  | "tri1" -> Tri1
  | "wand" -> Wand
  | "wire" -> Wire
  | "wor" -> Wor
  | "logic" -> Logic
  | "string" -> Sstring
  | s -> var_type_of_string s

(** The [scope_type] for any [$scope] type keyword, as the parser maps it. *)
let scope_type_of_keyword = function
  | "module" -> Module
  | "task" -> Task
  | "function" -> Function
  | "begin" -> Begin
  | "fork" -> Fork
  | s -> Other_scope s

(** Pack a binary string (MSB-first, left-to-right) into a [bytes] value. Within each byte the MSB is bit 7. If
    [len mod 8 <> 0] the first byte is left-padded with zeros, so the bytes are the value as a big-endian number. *)
let pack_bits s len =
  let nbytes = (len + 7) / 8 in
  let buf = Bytes.create nbytes in
  let pos = ref 0 in
  for b = 0 to nbytes - 1 do
    let v = ref 0 in
    for _ = 1 to if b = 0 then len - ((nbytes - 1) * 8) else 8 do
      v := (!v lsl 1) lor if s.[!pos] = '1' then 1 else 0;
      incr pos
    done;
    Bytes.unsafe_set buf b (Char.unsafe_chr !v)
  done;
  buf

(** Decode the raw binary string that follows [b]/[B] in a VCD value-change record into the richest applicable [value]
    variant.

    Classification:
    - all digits ['0'|'1'], length <= 24 → [Int]
    - all digits ['0'|'1'], 25..64 → [Int64]
    - all digits ['0'|'1'], > 64 → [Bytes]
    - all IEEE 1164 9-state digits ([0 1 x z u w l h -], either case) → [Scalars]
    - anything else (e.g. ['?']) → [Other]

    The per-event string length is intentionally not stored; look up {!var_decl.size} in the header for the canonical
    signal width. *)
let parse_vector s =
  let open Value in
  let len = String.length s in
  if String.for_all (fun c -> c = '0' || c = '1') s then
    if len <= int_bits then Int (String.fold_left (fun acc c -> (acc lsl 1) lor (Char.code c - Char.code '0')) 0 s)
    else if len <= 64 then
      Int64
        (String.fold_left (fun acc c -> Int64.logor (Int64.shift_left acc 1) (Int64.of_int (Char.code c land 1))) 0L s)
    else Bytes (pack_bits s len)
  else try Scalars (List.init len (fun i -> char_to_logic_exn s.[i])) with InvalidLogicChar _ -> Other s

exception Not_binary

(* The digit at [i] as 0 or 1; raises [Not_binary] for any other character. *)
let bit s i =
  let d = Char.code s.[i] - 48 in
  if d lor 1 <> 1 then raise_notrace Not_binary else d

(* The digits [i, j) as a native int; [j - i] must not exceed 62. *)
let bits_int s i j =
  let acc = ref 0 in
  for k = i to j - 1 do
    acc := (!acc lsl 1) lor bit s k
  done;
  !acc

(** The digits of a [b] record as the lexer decodes them: all-0/1 digits are an [Int] up to [int_bits], an [Int64] up to
    64 bits (both through the {!Value.int} and {!Value.int64} constructors), [Bytes] beyond; anything else goes through
    {!parse_vector}. *)
let binary_value s =
  let n = String.length s in
  if n = 0 then parse_vector s
  else
    try
      if n <= Value.int_bits then Value.int (bits_int s 0 n)
      else if n <= 64 then begin
        let split = max 0 (n - 32) in
        let hi = bits_int s 0 split and lo = bits_int s split n in
        Value.int64 (Int64.logor (Int64.shift_left (Int64.of_int hi) 32) (Int64.of_int lo))
      end
      else begin
        for i = 0 to n - 1 do
          ignore (bit s i)
        done;
        Value.bytes (pack_bits s n)
      end
    with Not_binary -> parse_vector s

(** The [width] digits packed MSB first from [off] in [b] (as FST stores 0/1 vectors: the last byte's low bits unused),
    as {!binary_value} of the digits would give them. *)
let packed_value b off width =
  let nbytes = (width + 7) / 8 in
  let pad = (nbytes * 8) - width in
  let byte i = Char.code (Bytes.unsafe_get b (off + i)) in
  if width <= Value.int_bits then begin
    let acc = ref 0 in
    for i = 0 to nbytes - 1 do
      acc := (!acc lsl 8) lor byte i
    done;
    Value.int (!acc lsr pad)
  end
  else if width <= 64 then begin
    let acc = ref 0L in
    for i = 0 to nbytes - 1 do
      acc := Int64.logor (Int64.shift_left !acc 8) (Int64.of_int (byte i))
    done;
    Value.int64 (Int64.shift_right_logical !acc pad)
  end
  else if pad = 0 then Value.bytes (Bytes.sub b off nbytes)
  else begin
    let out = Bytes.create nbytes in
    for i = 0 to nbytes - 1 do
      let prev = if i = 0 then 0 else byte (i - 1) in
      Bytes.unsafe_set out i (Char.unsafe_chr ((prev lsl (8 - pad)) lor (byte i lsr pad) land 0xff))
    done;
    Value.bytes out
  end

(** A scalar value change: one of the 9-state characters, or ['?'] (kept as [Other]). *)
let scalar_of_char = function '?' -> Value.Other "?" | c -> Value.Scalar (Value.char_to_logic_exn c)

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
