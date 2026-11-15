{
  open Vcd_parser

  let keywords = [
    "$end",              KW_END;
    "$enddefinitions",   KW_ENDDEFS;
    "$scope",            KW_SCOPE;
    "$upscope",          KW_UPSCOPE;
    "$var",              KW_VAR;
    "$timezero",         KW_TIMEZERO;
  ]

  let keyword_tbl =
    let h = Hashtbl.create 64 in
    List.iter (fun (k, v) -> Hashtbl.add h k v) keywords;
    h

  let lookup_or_id s =
    match Hashtbl.find_opt keyword_tbl s with
    | Some tok -> tok
    | None     -> ID s

  let mkid = Vcd_types.ID.of_string

  let newlines lexbuf s = String.iter (fun c -> if c = '\n' then Lexing.new_line lexbuf) s

  let unexpected lexbuf c =
    let p = Lexing.lexeme_start_p lexbuf in
    Vcd_error.located p (Printf.sprintf "unexpected %C" c)
}

(* ------------------------------------------------------------------ *)
(*  Character classes                                                  *)
(* ------------------------------------------------------------------ *)
let white   = [' ' '\t' '\r' '\n']
let digit   = ['0'-'9']

(* IEEE 1164 9-state chars, plus the '?' GTKWave writes for unknown states *)
let scalarchar = ['0' '1' 'x' 'X' 'z' 'Z' 'u' 'U' 'w' 'W' 'l' 'L' 'h' 'H' '-' '?']
let vecchar = scalarchar
let blank = [' ' '\t' '\r']

let bin_pfx = ['b' 'B']
let digit_01 = ['0' '1']

let idchar  = [^ ' ' '\t' '\r' '\n']
let word    = idchar+

(* ------------------------------------------------------------------ *)
(*  Header lexer — produces tokens consumed by Menhir                 *)
(* ------------------------------------------------------------------ *)
rule token = parse
  | '\n'    { Lexing.new_line lexbuf; token lexbuf }
  | blank+  { token lexbuf }
  | "$timescale" { TIMESCALE (collect_text (Buffer.create 16) lexbuf) }
  | "$date"      { DATE (collect_text (Buffer.create 16) lexbuf) }
  | "$version"   { VERSION (collect_text (Buffer.create 16) lexbuf) }
  | "$comment"   { COMMENT (collect_text (Buffer.create 16) lexbuf) }
  | word    { lookup_or_id (Lexing.lexeme lexbuf) }
  | eof     { Vcd_parser.EOF }
  | _       { token lexbuf }

(* ------------------------------------------------------------------ *)
(*  Free-text collector: reads words until $end, returns trimmed text *)
(* ------------------------------------------------------------------ *)
and collect_text buf = parse
  | white+ as w    { newlines lexbuf w; Buffer.add_char buf ' '; collect_text buf lexbuf }
  | "$end"         { String.trim (Buffer.contents buf) }
  | word as w      { Buffer.add_string buf w; collect_text buf lexbuf }
  | eof            { Vcd_error.located lexbuf.Lexing.lex_curr_p "unexpected EOF inside keyword block" }
  | _              { collect_text buf lexbuf }

(* ------------------------------------------------------------------ *)
(*  Simulation-section step lexer                                      *)
(*  Returns one event per call, or None at end-of-file.               *)
(* ------------------------------------------------------------------ *)
and next_selected keep = parse
  | '\n'
      { Lexing.new_line lexbuf; next_selected keep lexbuf }
  | blank+
      { next_selected keep lexbuf }

  | '#' (digit+ as n)
      { match Int64.of_string_opt n with
        | Some t -> Some (Vcd_ast.Timestamp (Vcd_types.Timestamp.of_int64 t))
        | None -> Vcd_error.located (Lexing.lexeme_start_p lexbuf) ("invalid timestamp " ^ n) }

  (* Scalar: logic char immediately followed by id chars (no space) *)
  | (scalarchar as v) (idchar+ as id)
      { if not (keep id) then next_selected keep lexbuf else Some (Vcd_ast.Change (mkid id, Vcd_ast.scalar_of_char v)) }

  (* All-0/1 vectors of any width.
     String.length is unavoidable here; ocamllex cannot count in a pattern.
     <= 24 → Int, 25..64 → Int64, > 64 → Bytes. *)
  | bin_pfx (digit_01+ as bits) white+ (word as id)
      { if not (keep id) then next_selected keep lexbuf else Some (Vcd_ast.Change (mkid id, Vcd_ast.binary_value bits)) }

  (* 4-state / 9-state slow path (x, z, u, …). *)
  | bin_pfx (vecchar+ as bits) white+ (word as id)
      { if not (keep id) then next_selected keep lexbuf else Some (Vcd_ast.Change (mkid id, Vcd_ast.parse_vector bits)) }

  (* Vector real: r<float> <id> *)
  | ['r' 'R'] ([^ ' ' '\t' '\r' '\n']+ as r) white+ (word as id)
      { if not (keep id) then next_selected keep lexbuf else let v = match float_of_string_opt r with
          | Some f -> Vcd_types.Value.Real f
          | None   -> Vcd_types.Value.Other r
        in
        Some (Vcd_ast.Change (mkid id, v)) }

  (* String: s<text> <id>, possibly empty (SystemC/UVM state names, GTKWave strings) *)
  | ['s' 'S'] (idchar* as s) white+ (word as id)
      { if not (keep id) then next_selected keep lexbuf else Some (Vcd_ast.Change (mkid id, Vcd_types.Value.String s)) }

  (* Port value as GTKWave's fst2vcd writes it: p<value> <id> *)
  | ['p' 'P'] (idchar+ as v) white+ (word as id)
      { if not (keep id) then next_selected keep lexbuf else Some (Vcd_ast.Change (mkid id, Vcd_types.Value.Port v)) }

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

  (* Any other $keyword: skip its block up to $end *)
  | '$' idchar+
      { ignore (collect_text (Buffer.create 64) lexbuf); next_selected keep lexbuf }

  | eof   { None }
  | _ as c { unexpected lexbuf c }

{
  let next_event lexbuf = next_selected (fun _ -> true) lexbuf
}
