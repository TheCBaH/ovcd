{
  open Vcd_parser

  let keywords = [
    "$end",              KW_END;
    "$enddefinitions",   KW_ENDDEFS;
    "$scope",            KW_SCOPE;
    "$upscope",          KW_UPSCOPE;
    "$var",              KW_VAR;
    "$timescale",        KW_TIMESCALE;
    "$date",             KW_DATE;
    "$version",          KW_VERSION;
    "$comment",          KW_COMMENT;
    "$dumpvars",         KW_DUMPVARS;
    "$dumpall",          KW_DUMPALL;
    "$dumpon",           KW_DUMPON;
    "$dumpoff",          KW_DUMPOFF;
    "$dumpports",        KW_DUMPPORTS;
    "module",    SCP_MODULE;
    "task",      SCP_TASK;
    "function",  SCP_FUNCTION;
    "begin",     SCP_BEGIN;
    "fork",      SCP_FORK;
    "event",     VT_EVENT;
    "integer",   VT_INTEGER;
    "parameter", VT_PARAMETER;
    "real",      VT_REAL;
    "realtime",  VT_REALTIME;
    "reg",       VT_REG;
    "supply0",   VT_SUPPLY0;
    "supply1",   VT_SUPPLY1;
    "time",      VT_TIME;
    "tri",       VT_TRI;
    "triand",    VT_TRIAND;
    "trior",     VT_TRIOR;
    "trireg",    VT_TRIREG;
    "tri0",      VT_TRI0;
    "tri1",      VT_TRI1;
    "wand",      VT_WAND;
    "wire",      VT_WIRE;
    "wor",       VT_WOR;
    "logic",     VT_LOGIC;    (* SystemVerilog / VHDL *)
    "string",    VT_STRING;   (* SystemVerilog / VHDL *)
  ]

  let keyword_tbl =
    let h = Hashtbl.create 64 in
    List.iter (fun (k, v) -> Hashtbl.add h k v) keywords;
    h

  let lookup_or_id s =
    match Hashtbl.find_opt keyword_tbl s with
    | Some tok -> tok
    | None     -> ID s

  (* Extract the integer value (0 or 1) of an ASCII binary digit.
     '0' = 0x30, '1' = 0x31 — the low bit is the digit value. *)
  let bit c = Char.code c land 1

  let nibble c1 c2 c3 c4 =
    bit c1 lsl 3 lor bit c2 lsl 2 lor bit c3 lsl 1 lor bit c4

  let byte c1 c2 c3 c4 c5 c6 c7 c8 =
    nibble c1 c2 c3 c4 lsl 4 lor nibble c5 c6 c7 c8

  let int16 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 =
    byte c1 c2 c3 c4 c5 c6 c7 c8 lsl 8 lor byte c9 c10 c11 c12 c13 c14 c15 c16

  let mkid = Vcd_types.ID.of_string
}

(* ------------------------------------------------------------------ *)
(*  Character classes                                                  *)
(* ------------------------------------------------------------------ *)
let white   = [' ' '\t' '\r' '\n']
let digit   = ['0'-'9']

(* 4-state logic chars used in scalar value changes *)
let scalarchar = ['0' '1' 'x' 'X' 'z' 'Z']

(* IEEE 1364/1800 extended vector chars: 4-state + 9-state + don't-care *)
let vecchar = ['0' '1' 'x' 'X' 'z' 'Z' 'u' 'U' 'w' 'W' 'l' 'L' 'h' 'H' '-']

let bin_pfx = ['b' 'B']
let digit_01 = ['0' '1']

let idchar  = [^ ' ' '\t' '\r' '\n']
let word    = idchar+

(* ------------------------------------------------------------------ *)
(*  Header lexer — produces tokens consumed by Menhir                 *)
(* ------------------------------------------------------------------ *)
rule token = parse
  | white+  { token lexbuf }
  | word    { lookup_or_id (Lexing.lexeme lexbuf) }
  | eof     { Vcd_parser.EOF }
  | _       { token lexbuf }

(* ------------------------------------------------------------------ *)
(*  Free-text collector: reads words until $end, returns trimmed text *)
(* ------------------------------------------------------------------ *)
and collect_text buf = parse
  | white+         { Buffer.add_char buf ' '; collect_text buf lexbuf }
  | "$end"         { String.trim (Buffer.contents buf) }
  | word as w      { Buffer.add_string buf w; collect_text buf lexbuf }
  | eof            { failwith "unexpected EOF inside keyword block" }
  | _              { collect_text buf lexbuf }

(* ------------------------------------------------------------------ *)
(*  Simulation-section step lexer                                      *)
(*  Returns one event per call, or None at end-of-file.               *)
(* ------------------------------------------------------------------ *)
and next_event = parse
  | white+
      { next_event lexbuf }

  | '#' (digit+ as n)
      { Some (Vcd_ast.Timestamp (Vcd_types.Timestamp.of_string n)) }

  (* Scalar: 4-state logic char immediately followed by id chars (no space) *)
  | (scalarchar as v) (idchar+ as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.Scalar (Vcd_types.Value.char_to_logic_exn v))) }

  (* All-0/1 vector fast paths — no String.length, no branch, no loop.
     Specific-length rules must precede the general rule so they win the
     longest-match tie for their exact lengths. *)

  (* 1 bit: b0 and b1 in one rule *)
  | bin_pfx (digit_01 as c) white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (bit c))) }

  (* 2 bits *)
  | bin_pfx (digit_01 as c1) (digit_01 as c2)
            white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (
            bit c1 lsl 1 lor bit c2))) }

  (* 3 bits *)
  | bin_pfx (digit_01 as c1) (digit_01 as c2) (digit_01 as c3)
            white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (
            bit c1 lsl 2 lor bit c2 lsl 1 lor bit c3))) }

  (* 4 bits *)
  | bin_pfx (digit_01 as c1) (digit_01 as c2) (digit_01 as c3) (digit_01 as c4)
            white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (nibble c1 c2 c3 c4))) }

  (* 8 bits *)
  | bin_pfx (digit_01 as c1) (digit_01 as c2) (digit_01 as c3) (digit_01 as c4)
            (digit_01 as c5) (digit_01 as c6) (digit_01 as c7) (digit_01 as c8)
            white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (byte c1 c2 c3 c4 c5 c6 c7 c8))) }

  (* 16 bits → Int (2^16−1 = 65535, fits in any OCaml int) *)
  | bin_pfx (digit_01 as c1)  (digit_01 as c2)  (digit_01 as c3)  (digit_01 as c4)
            (digit_01 as c5)  (digit_01 as c6)  (digit_01 as c7)  (digit_01 as c8)
            (digit_01 as c9)  (digit_01 as c10) (digit_01 as c11) (digit_01 as c12)
            (digit_01 as c13) (digit_01 as c14) (digit_01 as c15) (digit_01 as c16)
            white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (
            int16 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16))) }

  (* 24 bits → Int (2^24−1 = 16 777 215, still fits in the 31-bit minimum OCaml int) *)
  | bin_pfx (digit_01 as c1)  (digit_01 as c2)  (digit_01 as c3)  (digit_01 as c4)
            (digit_01 as c5)  (digit_01 as c6)  (digit_01 as c7)  (digit_01 as c8)
            (digit_01 as c9)  (digit_01 as c10) (digit_01 as c11) (digit_01 as c12)
            (digit_01 as c13) (digit_01 as c14) (digit_01 as c15) (digit_01 as c16)
            (digit_01 as c17) (digit_01 as c18) (digit_01 as c19) (digit_01 as c20)
            (digit_01 as c21) (digit_01 as c22) (digit_01 as c23) (digit_01 as c24)
            white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int (
            int16 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 lsl 8 lor
            byte c17 c18 c19 c20 c21 c22 c23 c24))) }

  (* 32 bits → Bytes (4 bytes, MSB first; 2^32−1 exceeds the 31-bit minimum OCaml int) *)
  | bin_pfx (digit_01 as c1)  (digit_01 as c2)  (digit_01 as c3)  (digit_01 as c4)
            (digit_01 as c5)  (digit_01 as c6)  (digit_01 as c7)  (digit_01 as c8)
            (digit_01 as c9)  (digit_01 as c10) (digit_01 as c11) (digit_01 as c12)
            (digit_01 as c13) (digit_01 as c14) (digit_01 as c15) (digit_01 as c16)
            (digit_01 as c17) (digit_01 as c18) (digit_01 as c19) (digit_01 as c20)
            (digit_01 as c21) (digit_01 as c22) (digit_01 as c23) (digit_01 as c24)
            (digit_01 as c25) (digit_01 as c26) (digit_01 as c27) (digit_01 as c28)
            (digit_01 as c29) (digit_01 as c30) (digit_01 as c31) (digit_01 as c32)
            white+ (word as id)
      { let hi = int16 c1  c2  c3  c4  c5  c6  c7  c8
                         c9  c10 c11 c12 c13 c14 c15 c16 in
        let lo = int16 c17 c18 c19 c20 c21 c22 c23 c24
                       c25 c26 c27 c28 c29 c30 c31 c32 in
        let v = Int64.logor (Int64.shift_left (Int64.of_int hi) 16)
                            (Int64.of_int lo) in
        Some (Vcd_ast.Change (mkid id, Vcd_types.Value.int64 v)) }

  (* General all-0/1 path — lengths not covered above (5,6,7,9–15,17–23,25–31,33+).
     String.length is unavoidable here; ocamllex cannot count in a pattern.
     <= 24 → Int, 25..64 → Int64, > 64 → Bytes. *)
  | bin_pfx (digit_01 digit_01+ as bits) white+ (word as id)
      { let n = String.length bits in
        let value =
          if n <= 24 then
            Vcd_types.Value.int
              (String.fold_left
                 (fun acc c -> (acc lsl 1) lor (Char.code c - 48)) 0 bits)
          else if n <= 64 then
            Vcd_types.Value.int64
              (String.fold_left
                 (fun acc c -> Int64.logor (Int64.shift_left acc 1)
                                 (Int64.of_int (Char.code c land 1))) 0L bits)
          else
            Vcd_types.Value.bytes (Vcd_ast.pack_bits bits n)
        in
        Some (Vcd_ast.Change (mkid id, value)) }

  (* 4-state / 9-state slow path (x, z, u, …). *)
  | bin_pfx (vecchar+ as bits) white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_ast.parse_vector bits)) }

  (* Vector real: r<float> <id> *)
  | ['r' 'R'] ([^ ' ' '\t' '\r' '\n']+ as r) white+ (word as id)
      { let v = match float_of_string_opt r with
          | Some f -> Vcd_types.Value.Real f
          | None   -> Vcd_types.Value.Other r
        in
        Some (Vcd_ast.Change (mkid id, v)) }

  (* State string (SystemC/UVM): s<name> <id> — kept as Other *)
  | ['s' 'S'] (idchar+ as s) white+ (word as id)
      { Some (Vcd_ast.Change (mkid id, Vcd_types.Value.Other s)) }

  (* $end terminates a dump block *)
  | "$end"
      { Some Vcd_ast.DumpEnd }

  | "$dumpvars"   { Some (Vcd_ast.DumpStart "dumpvars") }
  | "$dumpall"    { Some (Vcd_ast.DumpStart "dumpall") }
  | "$dumpon"     { Some (Vcd_ast.DumpStart "dumpon") }
  | "$dumpoff"    { Some (Vcd_ast.DumpStart "dumpoff") }
  | "$dumpports"  { Some (Vcd_ast.DumpStart "dumpports") }

  (* $comment: consume all text up to $end, return as SimComment *)
  | "$comment"
      { let buf = Buffer.create 64 in
        let text = collect_text buf lexbuf in
        Some (Vcd_ast.SimComment text) }

  | eof   { None }
  | _     { next_event lexbuf }
