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
%token KW_TIMEZERO

(* Text blocks arrive whole from the lexer, without their $end. *)
%token <string> DATE VERSION TIMESCALE COMMENT

(* Everything else is a word, including the scope and variable type names. *)
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
  | d  = DATE           { `Date d }
  | v  = VERSION        { `Version v }
  | ts = TIMESCALE      { `Timescale ts }
  | tz = timezero_decl  { `Timezero tz }
  | c  = COMMENT        { `Comment c }
  | s  = scope_decl     { `Scope s }
  | pre_header_sim      { `Skip }

(* Pre-header simulation content — some tools (e.g. logic-analyser exporters)
   emit a timestamp and/or a $dump* block before the $date/$scope declarations
   — and unknown top-level blocks such as $timezero or GTKWave's $attrbegin.
   Each rule consumes one token of such a construct:
   - A word covers timestamps (#0), value-change tokens (0!, b1010 …), the
     $dump* keywords and the words of unknown blocks.
   - $end closes a dump or unknown block. *)
pre_header_sim:
  | ID { () }
  | KW_END     { () }

(* GTKWave's time offset: a signed integer in timescale units. *)
timezero_decl:
  | KW_TIMEZERO w = ID KW_END
      { match Int64.of_string_opt w with
        | Some t -> t
        | None   -> Vcd_error.located $startpos ("invalid $timezero: " ^ w) }

(* ------------------------------------------------------------------ *)
(*  $scope … $upscope                                                  *)
(* ------------------------------------------------------------------ *)

scope_decl:
  | KW_SCOPE stype = scope_type sname = ID KW_END
    items = scope_item*
    KW_UPSCOPE KW_END
      { let vars, children = partition_items items in
        { s_type = stype; s_name = sname; vars; children } }

scope_type:
  | s = ID       { scope_type_of_keyword s }

scope_item:
  | v = var_decl      { `Var v }
  | s = scope_decl    { `Scope s }
  | scope_inner_skip  { `Skip }

(* Skip non-standard blocks that may appear inside a scope, e.g.
   $attrbegin misc 02 STD_LOGIC 1030 $end from VHDL-aware simulators, and
   $comment blocks (GTKWave writes FST comments where they occur). *)
scope_inner_skip:
  | ID { () }   (* $attrbegin keyword and its content tokens *)
  | COMMENT    { () }
  | KW_END     { () }   (* $end closing the block *)

(* ------------------------------------------------------------------ *)
(*  $var                                                               *)
(* ------------------------------------------------------------------ *)

var_decl:
  | KW_VAR vtype = var_type size = ID id = ID ref_ = ID idx = list(ID) KW_END
      { { v_type = vtype
        ; size   = (match int_of_string_opt size with
                    | Some n -> n
                    | None   -> Vcd_error.located $startpos ("invalid var size: " ^ size))
        ; id     = Vcd_types.ID.of_string id
        ; ref    = ref_
        ; index  = (match idx with [] -> None | l -> Some (String.concat " " l)) } }

var_type:
  | s = ID       { var_type_of_keyword s }
