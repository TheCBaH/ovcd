exception Parse_error of string

type parse_result = {
  header : Vcd_ast.header;  (** Parsed header / declaration section. *)
  simulation : Vcd_ast.event Seq.t;  (** Lazy sequence of simulation events; evaluated on demand. *)
}

(* ------------------------------------------------------------------ *)
(*  Internal helpers                                                   *)
(* ------------------------------------------------------------------ *)

let make_lexbuf_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "<channel>" };
  lexbuf

let make_lexbuf_from_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "<string>" };
  lexbuf

let make_lexbuf_from_file path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = path };
  (lexbuf, ic)

let parse_header_exn lexbuf =
  try Vcd_parser.parse_header Vcd_lexer.token lexbuf with
  | Vcd_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      raise
        (Parse_error
           (Printf.sprintf "syntax error at %s line %d col %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
              (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)))
  | Failure msg -> raise (Parse_error msg)

let events_seq lexbuf =
  let rec next () = match Vcd_lexer.next_event lexbuf with Some ev -> Seq.Cons (ev, next) | None -> Seq.Nil in
  next

let parse_lexbuf lexbuf =
  let header = parse_header_exn lexbuf in
  { header; simulation = events_seq lexbuf }

(* ------------------------------------------------------------------ *)
(*  Public API                                                         *)
(* ------------------------------------------------------------------ *)

let parse_string s = parse_lexbuf (make_lexbuf_from_string s)
let parse_channel ic = parse_lexbuf (make_lexbuf_from_channel ic)

let parse_file path =
  let lexbuf, _ic = make_lexbuf_from_file path in
  parse_lexbuf lexbuf

(* ------------------------------------------------------------------ *)
(*  ID → variable resolver                                            *)
(* ------------------------------------------------------------------ *)

module Resolver = struct
  type entry = { scope_type : Vcd_ast.scope_type; var : Vcd_ast.var_decl; reference : Vcd_types.Reference.t }

  module By_id = Map.Make (Vcd_types.ID)
  module By_ref = Map.Make (Vcd_types.Reference)

  type t = { by_id : entry By_id.t; by_ref : Vcd_types.ID.t By_ref.t }

  let make header =
    let open Vcd_ast in
    let open Vcd_types.Reference in
    let rec collect path scope_type vars children (by_id, by_ref) =
      let by_id, by_ref =
        List.fold_left
          (fun (by_id, by_ref) var ->
            let r = push var.ref path in
            (By_id.add var.id { scope_type; var; reference = r } by_id, By_ref.add r var.id by_ref))
          (by_id, by_ref) vars
      in
      List.fold_left
        (fun acc child -> collect (push child.s_name path) child.s_type child.vars child.children acc)
        (by_id, by_ref) children
    in
    let by_id, by_ref =
      List.fold_left
        (fun acc scope -> collect (push scope.s_name empty) scope.s_type scope.vars scope.children acc)
        (By_id.empty, By_ref.empty) header.scopes
    in
    { by_id; by_ref }

  let entry_id e = e.var.id
  let entry_size e = e.var.size
  let entry_reference e = e.reference
  let find t id = By_id.find_opt id t.by_id
  let reference t id = Option.map (fun e -> e.reference) (find t id)
  let find_id t ref = By_ref.find_opt ref t.by_ref
  let fold f t acc = By_id.fold (fun _ e a -> f e a) t.by_id acc
end

(* ------------------------------------------------------------------ *)
(*  Pretty-printing helpers                                            *)
(* ------------------------------------------------------------------ *)

let string_of_scope_type = function
  | Vcd_ast.Module -> "module"
  | Vcd_ast.Task -> "task"
  | Vcd_ast.Function -> "function"
  | Vcd_ast.Begin -> "begin"
  | Vcd_ast.Fork -> "fork"

let string_of_var_type = function
  | Vcd_ast.Event -> "event"
  | Vcd_ast.Integer -> "integer"
  | Vcd_ast.Parameter -> "parameter"
  | Vcd_ast.Real -> "real"
  | Vcd_ast.Realtime -> "realtime"
  | Vcd_ast.Reg -> "reg"
  | Vcd_ast.Supply0 -> "supply0"
  | Vcd_ast.Supply1 -> "supply1"
  | Vcd_ast.Time -> "time"
  | Vcd_ast.Tri -> "tri"
  | Vcd_ast.Triand -> "triand"
  | Vcd_ast.Trior -> "trior"
  | Vcd_ast.Trireg -> "trireg"
  | Vcd_ast.Tri0 -> "tri0"
  | Vcd_ast.Tri1 -> "tri1"
  | Vcd_ast.Wand -> "wand"
  | Vcd_ast.Wire -> "wire"
  | Vcd_ast.Wor -> "wor"
  | Vcd_ast.Logic -> "logic"
  | Vcd_ast.Sstring -> "string"
