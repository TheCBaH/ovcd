%{
  open Vcd_ast

  let partition_items items =
    List.fold_right
      (fun item (vars, children) ->
        match item with
        | `Var v   -> (v :: vars, children)
        | `Scope s -> (vars, s :: children)
        | `Skip    -> (vars, children))
      items ([], [])

  let build_header decls =
    let version   = ref None in
    let date      = ref None in
    let timescale = ref None in
    let timezero  = ref None in
    let comment   = ref None in
    let scopes    = ref [] in
    List.iter (function
      | `Version v   -> version   := Some v
      | `Date d      -> date      := Some d
      | `Timescale t -> timescale := Some t
      | `Timezero t  -> timezero  := Some t
      | `Comment c   -> comment   := Some c
      | `Scope s     -> scopes    := s :: !scopes
      | `Skip        -> ())
      decls;
    { version = !version; date = !date; timescale = !timescale; timezero = !timezero;
      comment = !comment; scopes = List.rev !scopes }
%}

(* ------------------------------------------------------------------ *)
(*  Tokens                                                             *)
(* ------------------------------------------------------------------ *)

%token KW_END KW_ENDDEFS
%token KW_SCOPE KW_UPSCOPE KW_VAR
%token KW_TIMESCALE KW_TIMEZERO KW_DATE KW_VERSION KW_COMMENT
%token KW_DUMPVARS KW_DUMPALL KW_DUMPON KW_DUMPOFF KW_DUMPPORTS

%token SCP_MODULE SCP_TASK SCP_FUNCTION SCP_BEGIN SCP_FORK

%token VT_EVENT VT_INTEGER VT_PARAMETER VT_REAL VT_REALTIME VT_REG
%token VT_SUPPLY0 VT_SUPPLY1 VT_TIME VT_TRI VT_TRIAND VT_TRIOR VT_TRIREG
%token VT_TRI0 VT_TRI1 VT_WAND VT_WIRE VT_WOR
%token VT_LOGIC VT_STRING

%token <string> ID
%token EOF

(* ------------------------------------------------------------------ *)
(*  Entry point                                                        *)
(* ------------------------------------------------------------------ *)

%start <Vcd_ast.header> parse_header

%%

(* ------------------------------------------------------------------ *)
(*  Top-level                                                          *)
(* ------------------------------------------------------------------ *)

parse_header:
  | decls = declaration* KW_ENDDEFS KW_END { build_header decls }
  | decls = declaration* EOF               { build_header decls }

(* ------------------------------------------------------------------ *)
(*  Declarations                                                       *)
(* ------------------------------------------------------------------ *)

declaration:
  | d  = date_decl      { `Date d }
  | v  = version_decl   { `Version v }
  | ts = timescale_decl { `Timescale ts }
  | tz = timezero_decl  { `Timezero tz }
  | c  = comment_decl   { `Comment c }
  | s  = scope_decl     { `Scope s }
  | pre_header_sim      { `Skip }

(* Pre-header simulation content — some tools (e.g. logic-analyser exporters)
   emit a timestamp and/or a $dump* block before the $date/$scope declarations
   — and unknown top-level blocks such as $timezero or GTKWave's $attrbegin.
   Each rule consumes one token of such a construct:
   - A word covers timestamps (#0), value-change tokens (0!, b1010 …), the
     $dump* keywords and the words of unknown blocks, keywords included.
   - $end closes a dump or unknown block. *)
pre_header_sim:
  | word_token { () }
  | KW_END     { () }

date_decl:
  | KW_DATE ws = nonempty_list(word_token) KW_END { String.concat " " ws }

version_decl:
  | KW_VERSION ws = nonempty_list(word_token) KW_END { String.concat " " ws }

(* GTKWave's time offset: a signed integer in timescale units. *)
timezero_decl:
  | KW_TIMEZERO w = word_token KW_END
      { match Int64.of_string_opt w with
        | Some t -> t
        | None   -> failwith ("invalid $timezero: " ^ w) }

timescale_decl:
  | KW_TIMESCALE ws = nonempty_list(word_token) KW_END { String.concat " " ws }

comment_decl:
  | KW_COMMENT ws = list(word_token) KW_END { String.concat " " ws }

(* Any non-$end token that looks like a word — used both in free-text
   blocks and in identifier positions (scope names, var references) so
   that VCD signal names that happen to match keywords still parse. *)
ident:
  | s = ID      { s }
  | SCP_MODULE  { "module" }   | SCP_TASK     { "task" }
  | SCP_FUNCTION{ "function" } | SCP_BEGIN    { "begin" }
  | SCP_FORK    { "fork" }
  | VT_EVENT    { "event" }    | VT_INTEGER   { "integer" }
  | VT_PARAMETER{ "parameter" }| VT_REAL      { "real" }
  | VT_REALTIME { "realtime" } | VT_REG       { "reg" }
  | VT_SUPPLY0  { "supply0" }  | VT_SUPPLY1   { "supply1" }
  | VT_TIME     { "time" }     | VT_TRI       { "tri" }
  | VT_TRIAND   { "triand" }   | VT_TRIOR     { "trior" }
  | VT_TRIREG   { "trireg" }   | VT_TRI0      { "tri0" }
  | VT_TRI1     { "tri1" }     | VT_WAND      { "wand" }
  | VT_WIRE     { "wire" }     | VT_WOR       { "wor" }
  | VT_LOGIC    { "logic" }    | VT_STRING    { "string" }
  | KW_DUMPVARS { "dumpvars" } | KW_DUMPALL   { "dumpall" }
  | KW_DUMPON   { "dumpon" }   | KW_DUMPOFF   { "dumpoff" }
  | KW_DUMPPORTS{ "dumpports" }

word_token:
  | s = ID      { s }
  | SCP_MODULE  { "module" }   | SCP_TASK     { "task" }
  | SCP_FUNCTION{ "function" } | SCP_BEGIN    { "begin" }
  | SCP_FORK    { "fork" }
  | VT_EVENT    { "event" }    | VT_INTEGER   { "integer" }
  | VT_PARAMETER{ "parameter" }| VT_REAL      { "real" }
  | VT_REALTIME { "realtime" } | VT_REG       { "reg" }
  | VT_SUPPLY0  { "supply0" }  | VT_SUPPLY1   { "supply1" }
  | VT_TIME     { "time" }     | VT_TRI       { "tri" }
  | VT_TRIAND   { "triand" }   | VT_TRIOR     { "trior" }
  | VT_TRIREG   { "trireg" }   | VT_TRI0      { "tri0" }
  | VT_TRI1     { "tri1" }     | VT_WAND      { "wand" }
  | VT_WIRE     { "wire" }     | VT_WOR       { "wor" }
  | VT_LOGIC    { "logic" }    | VT_STRING    { "string" }
  | KW_DUMPVARS { "dumpvars" } | KW_DUMPALL   { "dumpall" }
  | KW_DUMPON   { "dumpon" }   | KW_DUMPOFF   { "dumpoff" }
  | KW_DUMPPORTS{ "dumpports" }

(* ------------------------------------------------------------------ *)
(*  $scope … $upscope                                                  *)
(* ------------------------------------------------------------------ *)

scope_decl:
  | KW_SCOPE stype = scope_type sname = ident KW_END
    items = scope_item*
    KW_UPSCOPE KW_END
      { let vars, children = partition_items items in
        { s_type = stype; s_name = sname; vars; children } }

scope_type:
  | SCP_MODULE   { Module }
  | SCP_TASK     { Task }
  | SCP_FUNCTION { Function }
  | SCP_BEGIN    { Begin }
  | SCP_FORK     { Fork }
  | s = ID       { Other_scope s }   (* non-standard: struct, vhdl_architecture, … *)

scope_item:
  | v = var_decl      { `Var v }
  | s = scope_decl    { `Scope s }
  | scope_inner_skip  { `Skip }

(* Skip non-standard blocks that may appear inside a scope, e.g.
   $attrbegin misc 02 STD_LOGIC 1030 $end from VHDL-aware simulators, and
   $comment blocks (GTKWave writes FST comments where they occur). *)
scope_inner_skip:
  | word_token { () }   (* $attrbegin keyword and its content tokens *)
  | KW_COMMENT { () }
  | KW_END     { () }   (* $end closing the block *)

(* ------------------------------------------------------------------ *)
(*  $var                                                               *)
(* ------------------------------------------------------------------ *)

var_decl:
  | KW_VAR vtype = var_type size = ident id = ident ref_ = ident idx = option(ident) KW_END
      { { v_type = vtype
        ; size   = (match int_of_string_opt size with
                    | Some n -> n
                    | None   -> failwith ("invalid var size: " ^ size))
        ; id     = Vcd_types.ID.of_string id
        ; ref    = ref_
        ; index  = idx } }

var_type:
  | VT_EVENT     { Event }     | VT_INTEGER   { Integer }
  | VT_PARAMETER { Parameter } | VT_REAL      { Real }
  | VT_REALTIME  { Realtime }  | VT_REG       { Reg }
  | VT_SUPPLY0   { Supply0 }   | VT_SUPPLY1   { Supply1 }
  | VT_TIME      { Time }      | VT_TRI       { Tri }
  | VT_TRIAND    { Triand }    | VT_TRIOR     { Trior }
  | VT_TRIREG    { Trireg }    | VT_TRI0      { Tri0 }
  | VT_TRI1      { Tri1 }      | VT_WAND      { Wand }
  | VT_WIRE      { Wire }      | VT_WOR       { Wor }
  | VT_LOGIC     { Logic }     | VT_STRING    { Sstring }
  | s = ID       { var_type_of_string s }   (* bit, int, …, or Other_type *)
