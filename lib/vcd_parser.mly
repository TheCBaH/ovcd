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
    let comment   = ref None in
    let scopes    = ref [] in
    List.iter (function
      | `Version v   -> version   := Some v
      | `Date d      -> date      := Some d
      | `Timescale t -> timescale := Some t
      | `Comment c   -> comment   := Some c
      | `Scope s     -> scopes    := s :: !scopes
      | `Skip        -> ())
      decls;
    { version = !version; date = !date; timescale = !timescale;
      comment = !comment; scopes = List.rev !scopes }
%}

(* ------------------------------------------------------------------ *)
(*  Tokens                                                             *)
(* ------------------------------------------------------------------ *)

%token KW_END KW_ENDDEFS
%token KW_SCOPE KW_UPSCOPE KW_VAR
%token KW_TIMESCALE KW_DATE KW_VERSION KW_COMMENT
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
  | c  = comment_decl   { `Comment c }
  | s  = scope_decl     { `Scope s }
  | pre_header_sim      { `Skip }

(* Pre-header simulation content — some tools (e.g. logic-analyser exporters)
   emit a timestamp and/or a $dump* block before the $date/$scope declarations.
   Each rule consumes one such construct:
   - A bare ID covers timestamps (#0) and value-change tokens (0!, b1010 …)
     as they appear in the header token stream.
   - A dump_kw … [$end] rule covers $dumpvars/$dumpon/… blocks whose closing
     $end may be absent when the block has no content. *)
pre_header_sim:
  | ID      { () }   (* timestamp (#0) or value token (0!, b1010 …) *)
  | dump_kw { () }   (* $dump* keyword; its content follows as ID tokens *)
  | KW_END  { () }   (* $end closing the pre-header dump block *)

dump_kw:
  | KW_DUMPVARS  { () }
  | KW_DUMPALL   { () }
  | KW_DUMPON    { () }
  | KW_DUMPOFF   { () }
  | KW_DUMPPORTS { () }

date_decl:
  | KW_DATE ws = nonempty_list(word_token) KW_END { String.concat " " ws }

version_decl:
  | KW_VERSION ws = nonempty_list(word_token) KW_END { String.concat " " ws }

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
  | ID           { Module }   (* non-standard: vhdl_architecture, vhdl_record, … *)

scope_item:
  | v = var_decl      { `Var v }
  | s = scope_decl    { `Scope s }
  | scope_inner_skip  { `Skip }

(* Skip non-standard blocks that may appear inside a scope, e.g.
   $attrbegin misc 02 STD_LOGIC 1030 $end from VHDL-aware simulators. *)
scope_inner_skip:
  | ID     { () }   (* $attrbegin keyword and its content tokens *)
  | KW_END { () }   (* $end closing the attribute block *)

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
