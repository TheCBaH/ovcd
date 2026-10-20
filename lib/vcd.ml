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

(* The selected IDs among those the header declares, each asked once, on the first pull. *)
let selection header select =
  let asked = Hashtbl.create 1024 and kept = Hashtbl.create 1024 in
  let rec walk (s : Vcd_ast.scope_node) =
    List.iter
      (fun (v : Vcd_ast.var_decl) ->
        let k = Vcd_types.ID.to_string v.id in
        if not (Hashtbl.mem asked k) then begin
          Hashtbl.replace asked k ();
          if select v.id then Hashtbl.replace kept k ()
        end)
      s.vars;
    List.iter walk s.children
  in
  List.iter walk header.Vcd_ast.scopes;
  Hashtbl.mem kept

let events_seq ?select header lexbuf =
  let chosen = ref None in
  let rec next () =
    let keep =
      match (!chosen, select) with
      | Some k, _ -> k
      | None, None -> Fun.const true
      | None, Some f ->
          let k = selection header f in
          chosen := Some k;
          k
    in
    match Vcd_lexer.next_selected keep lexbuf with
    | Some ev -> Seq.Cons (ev, next)
    | None -> Seq.Nil
    | exception Failure msg -> raise (Parse_error msg)
  in
  next

let parse_lexbuf ?select lexbuf =
  let header = parse_header_exn lexbuf in
  { header; simulation = events_seq ?select header lexbuf }

(* ------------------------------------------------------------------ *)
(*  Public API                                                         *)
(* ------------------------------------------------------------------ *)

let parse_string ?select s = parse_lexbuf ?select (make_lexbuf_from_string s)
let parse_channel ?select ic = parse_lexbuf ?select (make_lexbuf_from_channel ic)

let parse_file ?select path =
  let lexbuf, _ic = make_lexbuf_from_file path in
  parse_lexbuf ?select lexbuf

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

  type entry = { id : Vcd_types.ID.t; size : int; refs : Vcd_types.Reference.t list; references : Ref_set.t Lazy.t }

  let entry id size refs = { id; size; refs; references = lazy (Ref_set.of_list refs) }

  (* [Hashtbl.hash] only samples the leaf-most components of a path; [equal] decides, so deep paths merely collide. *)
  module Ref_tbl = Hashtbl.Make (struct
    type t = Vcd_types.Reference.t

    let equal = Vcd_types.Reference.equal
    let hash = Hashtbl.hash
  end)

  type ordered = {
    by_ref : Vcd_types.ID.t Ref_map.t;  (** Reversed reference order: leaf first. *)
    by_ref_fwd : Vcd_types.ID.t Ref_fwd_map.t;  (** Forward reference order: scope first. *)
  }

  type t = {
    by_id : entry ID_map.t;
    ids_of_ref : Vcd_types.ID.t Ref_tbl.t;
    ordered : ordered Lazy.t;  (** Built on the first prefix/suffix query. *)
  }

  let make header =
    let open Vcd_ast in
    let open Vcd_types.Reference in
    (* Sized to the header: growing the table was most of the cost of [make]. *)
    let rec count (s : scope_node) = List.fold_left (fun n c -> n + count c) (List.length s.vars) s.children in
    let ids_of_ref = Ref_tbl.create (List.fold_left (fun n s -> n + count s) 16 header.scopes) in
    let rec collect path vars children by_id =
      let by_id =
        List.fold_left
          (fun by_id var ->
            let r = push var.ref path in
            Ref_tbl.replace ids_of_ref r var.id;
            ID_map.update var.id
              (function None -> Some (entry var.id var.size [ r ]) | Some e -> Some (entry e.id e.size (r :: e.refs)))
              by_id)
          by_id vars
      in
      List.fold_left (fun acc child -> collect (push child.s_name path) child.vars child.children acc) by_id children
    in
    let by_id =
      List.fold_left
        (fun acc scope -> collect (push scope.s_name empty) scope.vars scope.children acc)
        ID_map.empty header.scopes
    in
    let ordered =
      lazy
        (Ref_tbl.fold
           (fun r id { by_ref; by_ref_fwd } ->
             { by_ref = Ref_map.add r id by_ref; by_ref_fwd = Ref_fwd_map.add (to_list r) id by_ref_fwd })
           ids_of_ref
           { by_ref = Ref_map.empty; by_ref_fwd = Ref_fwd_map.empty })
    in
    { by_id; ids_of_ref; ordered }

  let entry_id e = e.id
  let entry_size e = e.size
  let entry_references e = Lazy.force e.references
  let find t id = ID_map.find_opt id t.by_id

  let references t id =
    match ID_map.find_opt id t.by_id with None -> Ref_set.empty | Some e -> Lazy.force e.references

  let find_id t ref = Ref_tbl.find_opt t.ids_of_ref ref
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
    go ID_set.empty (Ref_map.to_seq_from lb (Lazy.force t.ordered).by_ref)

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
    go ID_set.empty (Ref_fwd_map.to_seq_from prefix_fwd (Lazy.force t.ordered).by_ref_fwd)

  let find_all pat t =
    let head, tail = Filter_matcher.anchors pat in
    let add_if_match e acc =
      let refs = Ref_set.filter (Filter_matcher.matches pat) (Lazy.force e.references) in
      if Ref_set.is_empty refs then acc
      else { e with refs = Ref_set.elements refs; references = Lazy.from_val refs } :: acc
    in
    match (head, tail) with
    | [], [] -> ID_map.fold (fun _ e acc -> add_if_match e acc) t.by_id []
    | _ ->
        let candidate_ids = match tail with [] -> collect_by_prefix head t | _ -> collect_by_tail tail t in
        ID_set.fold
          (fun id acc -> match ID_map.find_opt id t.by_id with None -> acc | Some e -> add_if_match e acc)
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

  let stream ?tracked ?reported ?(init : state = ID_map.empty) ?(ranges : time_range list = []) events =
    let ids =
      match (tracked, reported) with
      | None, None -> None
      | Some t, None -> Some t
      | None, Some r -> Some r
      | Some t, Some r -> Some (ID_set.union t r)
    in
    let in_ids id = match ids with None -> true | Some s -> ID_set.mem id s in
    let rec collect in_dump state filtered events =
      match events () with
      | Seq.Nil -> (state, filtered, `Nil)
      | Seq.Cons (Vcd_ast.Timestamp t, rest) -> (state, filtered, `Timestamp (t, rest))
      | Seq.Cons (Vcd_ast.DumpStart _, rest) -> collect true state filtered rest
      | Seq.Cons (Vcd_ast.DumpEnd, rest) -> collect false state filtered rest
      | Seq.Cons (Vcd_ast.Change (id, v), rest) ->
          let new_state = if in_ids id then ID_map.add id v state else state in
          let new_filtered = if (not in_dump) && in_ids id then ID_map.add id v filtered else filtered in
          collect in_dump new_state new_filtered rest
      | Seq.Cons (_, rest) -> collect in_dump state filtered rest
    in
    (* [was_in_range] = whether the previous timestep was inside any range.
       When transitioning from outside to inside (and ranges were specified),
       we emit a snapshot of the tracked+reported state so the consumer can see
       values that changed while the stream was outside the range. *)
    let rec loop was_in_range state t events () =
      if past_all_ranges ranges t then Seq.Nil
      else
        let new_state, filtered, term = collect false state ID_map.empty events in
        let in_range = in_ranges ranges t in
        let effective_changes = if in_range && (not was_in_range) && ranges <> [] then new_state else filtered in
        let emit = in_range && not (ID_map.is_empty effective_changes) in
        match term with
        | `Nil ->
            if emit then Seq.Cons ({ state; time = t; changes = effective_changes }, Fun.const Seq.Nil) else Seq.Nil
        | `Timestamp (next_t, remaining) ->
            if emit then
              Seq.Cons ({ state; time = t; changes = effective_changes }, loop in_range new_state next_t remaining)
            else loop in_range new_state next_t remaining ()
    in
    match collect false (ID_map.filter (fun id _ -> in_ids id) init) ID_map.empty events with
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
  | Vcd_ast.Other_scope s -> s

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
  | Vcd_ast.Bit -> "bit"
  | Vcd_ast.Byte -> "byte"
  | Vcd_ast.Enum -> "enum"
  | Vcd_ast.Int -> "int"
  | Vcd_ast.Longint -> "longint"
  | Vcd_ast.Port -> "port"
  | Vcd_ast.Real_parameter -> "real_parameter"
  | Vcd_ast.Shortint -> "shortint"
  | Vcd_ast.Shortreal -> "shortreal"
  | Vcd_ast.Sparray -> "sparray"
  | Vcd_ast.Other_type s -> s
