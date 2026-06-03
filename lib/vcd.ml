[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-sonnet-4-6"]
[@@@ai_provider "Anthropic"]

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
(*  Shared key-indexed containers                                     *)
(* ------------------------------------------------------------------ *)

module ID_map = Map.Make (Vcd_types.ID)
module ID_set = Set.Make (Vcd_types.ID)
module Ref_map = Map.Make (Vcd_types.Reference)
module Ref_set = Set.Make (Vcd_types.Reference)

(* ------------------------------------------------------------------ *)
(*  ID → variable resolver                                            *)
(* ------------------------------------------------------------------ *)

module Resolver = struct
  (* Forward-order reference map: key is the forward (scope-first) component list.
     Comparator is a plain list compare with no allocation, enabling efficient
     prefix queries for patterns like [counter_tb.**]. *)
  module Ref_fwd_map = Map.Make (struct
    type t = string list

    let compare = List.compare String.compare
  end)

  type entry = { id : Vcd_types.ID.t; size : int; references : Ref_set.t }

  type t = {
    by_id : entry ID_map.t;
    by_ref : Vcd_types.ID.t Ref_map.t;  (** Reversed reference order: leaf first. *)
    by_ref_fwd : Vcd_types.ID.t Ref_fwd_map.t;  (** Forward reference order: scope first. *)
  }

  let make header =
    let open Vcd_ast in
    let open Vcd_types.Reference in
    let rec collect path vars children (by_id, by_ref, by_ref_fwd) =
      let by_id, by_ref, by_ref_fwd =
        List.fold_left
          (fun (by_id, by_ref, by_ref_fwd) var ->
            let r = push var.ref path in
            let entry =
              match ID_map.find_opt var.id by_id with
              | None -> { id = var.id; size = var.size; references = Ref_set.singleton r }
              | Some e -> { e with references = Ref_set.add r e.references }
            in
            (ID_map.add var.id entry by_id, Ref_map.add r var.id by_ref, Ref_fwd_map.add (to_list r) var.id by_ref_fwd))
          (by_id, by_ref, by_ref_fwd) vars
      in
      List.fold_left
        (fun acc child -> collect (push child.s_name path) child.vars child.children acc)
        (by_id, by_ref, by_ref_fwd) children
    in
    let by_id, by_ref, by_ref_fwd =
      List.fold_left
        (fun acc scope -> collect (push scope.s_name empty) scope.vars scope.children acc)
        (ID_map.empty, Ref_map.empty, Ref_fwd_map.empty)
        header.scopes
    in
    { by_id; by_ref; by_ref_fwd }

  let entry_id e = e.id
  let entry_size e = e.size
  let entry_references e = e.references
  let find t id = ID_map.find_opt id t.by_id
  let references t id = match ID_map.find_opt id t.by_id with None -> Ref_set.empty | Some e -> e.references
  let find_id t ref = Ref_map.find_opt ref t.by_ref
  let fold f t acc = ID_map.fold (fun _ e a -> f e a) t.by_id acc

  let list_starts_with prefix lst =
    let rec go = function
      | [], _ -> true
      | _ :: _, [] -> false
      | ph :: pt, lh :: lt -> String.equal ph lh && go (pt, lt)
    in
    go (prefix, lst)

  (* Range-query [by_ref] for references ending with [tail_fwd].
     [by_ref] sorts by the reversed internal representation (leaf first), so
     [lb = Reference.of_list tail_fwd] (internal form = List.rev tail_fwd) is the
     lower bound of the target interval.  The upper bound [ub] is derived by
     appending "\x00" to the first component of [tail_fwd]: every string in the
     half-open interval [lb, ub) ends with [tail_fwd] in forward order, with no
     [to_list] call inside the loop. *)
  let collect_by_tail tail_fwd t =
    let lb = Vcd_types.Reference.of_list tail_fwd in
    let ub = Vcd_types.Reference.of_list ((List.hd tail_fwd ^ "\x00") :: List.tl tail_fwd) in
    let rec go acc s =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons ((k, id), rest) -> if Vcd_types.Reference.compare k ub < 0 then go (ID_set.add id acc) rest else acc
    in
    go ID_set.empty (Ref_map.to_seq_from lb t.by_ref)

  (* Range-query [by_ref_fwd] for references starting with [prefix_fwd].
     Keys are [string list] in forward order, so [prefix_fwd] is used directly
     as the lower bound with no conversion, and the per-element check is a
     plain [list_starts_with] with no allocation. *)
  let collect_by_prefix prefix_fwd t =
    let rec go acc s =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons ((k, id), rest) -> if list_starts_with prefix_fwd k then go (ID_set.add id acc) rest else acc
    in
    go ID_set.empty (Ref_fwd_map.to_seq_from prefix_fwd t.by_ref_fwd)

  let find_all pat t =
    let head, tail = Filter_matcher.anchors pat in
    match (head, tail) with
    | [], [] ->
        ID_map.fold
          (fun _ e acc -> if Ref_set.exists (Filter_matcher.matches pat) e.references then e :: acc else acc)
          t.by_id []
    | _ ->
        let candidate_ids = match tail with [] -> collect_by_prefix head t | _ -> collect_by_tail tail t in
        ID_set.fold
          (fun id acc ->
            match ID_map.find_opt id t.by_id with
            | None -> acc
            | Some e -> if Ref_set.exists (Filter_matcher.matches pat) e.references then e :: acc else acc)
          candidate_ids []
end

(* ------------------------------------------------------------------ *)
(*  Time range                                                         *)
(* ------------------------------------------------------------------ *)

type time_range = {
  start : Vcd_types.Timestamp.t option;  (** Inclusive lower bound; [None] means "from the beginning". *)
  stop : Vcd_types.Timestamp.t option;  (** Inclusive upper bound; [None] means "to the end". *)
}

(* ------------------------------------------------------------------ *)
(*  Time-range helpers                                                 *)
(* ------------------------------------------------------------------ *)

let in_ranges ranges t =
  match ranges with
  | [] -> true
  | _ ->
      List.exists
        (fun r ->
          (match r.start with None -> true | Some s -> Vcd_types.Timestamp.compare t s >= 0)
          && match r.stop with None -> true | Some e -> Vcd_types.Timestamp.compare t e <= 0)
        ranges

let past_all_ranges ranges t =
  match ranges with
  | [] -> false
  | _ ->
      List.for_all (fun r -> match r.stop with None -> false | Some e -> Vcd_types.Timestamp.compare t e > 0) ranges

(* ------------------------------------------------------------------ *)
(*  Stateful stream                                                    *)
(* ------------------------------------------------------------------ *)

module Stateful = struct
  type state = Vcd_types.Value.t ID_map.t

  let find state id = ID_map.find_opt id state

  type event = { state : state; time : Vcd_types.Timestamp.t; changes : state }

  let stream ?tracked ?reported ?(ranges : time_range list = []) events =
    let is_tracked id = match tracked with None -> true | Some s -> ID_set.mem id s in
    let is_reported id = match reported with None -> true | Some s -> ID_set.mem id s in
    let rec collect in_dump state filtered events =
      match events () with
      | Seq.Nil -> (state, filtered, `Nil)
      | Seq.Cons (Vcd_ast.Timestamp t, rest) -> (state, filtered, `Timestamp (t, rest))
      | Seq.Cons (Vcd_ast.DumpStart _, rest) -> collect true state filtered rest
      | Seq.Cons (Vcd_ast.DumpEnd, rest) -> collect false state filtered rest
      | Seq.Cons (Vcd_ast.Change (id, v), rest) ->
          let new_state = if is_tracked id then ID_map.add id v state else state in
          let new_filtered = if (not in_dump) && is_reported id then ID_map.add id v filtered else filtered in
          collect in_dump new_state new_filtered rest
      | Seq.Cons (_, rest) -> collect in_dump state filtered rest
    in
    (* [was_in_range] = whether the previous timestep was inside any range.
       When transitioning from outside to inside (and ranges were specified),
       we emit a snapshot of the full reported state so the consumer can see
       values that changed while the stream was outside the range. *)
    let rec loop was_in_range state t events () =
      if past_all_ranges ranges t then Seq.Nil
      else
        let new_state, filtered, term = collect false state ID_map.empty events in
        let in_range = in_ranges ranges t in
        let effective_changes =
          if in_range && (not was_in_range) && ranges <> [] then ID_map.filter (fun id _ -> is_reported id) new_state
          else filtered
        in
        let emit = in_range && not (ID_map.is_empty effective_changes) in
        match term with
        | `Nil ->
            if emit then Seq.Cons ({ state; time = t; changes = effective_changes }, Fun.const Seq.Nil) else Seq.Nil
        | `Timestamp (next_t, remaining) ->
            if emit then
              Seq.Cons ({ state; time = t; changes = effective_changes }, loop in_range new_state next_t remaining)
            else loop in_range new_state next_t remaining ()
    in
    match collect false ID_map.empty ID_map.empty events with
    | _, _, `Nil -> Fun.const Seq.Nil
    | pre_state, _, `Timestamp (first_t, first_events) -> loop false pre_state first_t first_events
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
