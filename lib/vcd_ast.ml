open Vcd_types

type scope_type = Module | Task | Function | Begin | Fork

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

(*
let char_to_logic = function
  | '0' -> B0
  | '1' -> B1
  | 'x' | 'X' -> X
  | 'z' | 'Z' -> Z
  | c -> failwith (Printf.sprintf "invalid logic char: %c" c)
*)

(** Pack a binary string (MSB-first, left-to-right) into a [bytes] value. Within each byte the MSB is bit 7. If
    [len mod 8 <> 0] the last byte is right-padded with zeros. *)
let pack_bits s len =
  let nbytes = (len + 7) / 8 in
  let buf = Bytes.make nbytes '\x00' in
  for i = 0 to len - 1 do
    if s.[i] = '1' then begin
      let byte_idx = i / 8 in
      let bit_pos = 7 - (i mod 8) in
      Bytes.set buf byte_idx (Char.chr (Char.code (Bytes.get buf byte_idx) lor (1 lsl bit_pos)))
    end
  done;
  buf

(** Decode the raw binary string that follows [b]/[B] in a VCD value-change record into the richest applicable [value]
    variant.

    Classification:
    - all digits ['0'|'1'], length <= 24 → [Int]
    - all digits ['0'|'1'], 25..64 → [Int64]
    - all digits ['0'|'1'], > 64 → [Bytes]
    - all 4-state digits (['0'|'1'|'x'|'X'|'z'|'Z']) → [Scalars]
    - anything else (9-state, etc.) → [Other]

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
  comment : string option;
  scopes : scope_node list;
}

type event =
  | Timestamp of Vcd_types.Timestamp.t
  | Change of Vcd_types.ID.t * Value.t  (** (id_code, new_value) *)
  | DumpStart of string  (** "$dumpvars" / "$dumpall" / … *)
  | DumpEnd  (** "$end" closing a dump block *)
  | SimComment of string
