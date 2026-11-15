[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5-5"]
[@@@ai_provider "Anthropic"]

open Vcd_ast
module Value = Vcd_types.Value

type format = [ `Vcd | `Fst ]

(* The header block's type byte and its 64-bit big-endian length, 329. *)
let header_block = "\x00\x00\x00\x00\x00\x00\x00\x01\x49"

(* A wrapper's type byte, then 16 bytes of lengths, then gzip (or, as gzread allows, stored) data. *)
let detect_string s : format =
  let n = String.length s in
  let at off p = n >= off + String.length p && String.sub s off (String.length p) = p in
  if at 0 header_block then `Fst
  else if n > 0 && s.[0] = '\xfe' && (at 17 "\x1f\x8b" || at 17 header_block) then `Fst
  else `Vcd

let detect_file path =
  In_channel.with_open_bin path (fun ic ->
      let b = Bytes.create (17 + String.length header_block) in
      let rec fill n =
        if n = Bytes.length b then n
        else match In_channel.input ic b n (Bytes.length b - n) with 0 -> n | k -> fill (n + k)
      in
      detect_string (Bytes.sub_string b 0 (fill 0)))

(* Text as the header grammar reads it: whitespace-separated words joined by single spaces. *)
let words s =
  String.map (function '\t' | '\r' | '\n' -> ' ' | c -> c) s |> String.split_on_char ' ' |> List.filter (( <> ) "")

let text s = match words s with [] -> None | ws -> Some (String.concat " " ws)

type open_scope = {
  st : scope_type;
  name : string;
  mutable o_vars : var_decl list;
  mutable o_children : scope_node list;
}

let close_scope o = { s_type = o.st; s_name = o.name; vars = List.rev o.o_vars; children = List.rev o.o_children }

let ids_of t =
  match Fst.signals t with
  | Ok sigs -> Ok (Array.init (Array.length sigs) (fun i -> Vcd_types.ID.of_string (Fst.Vcd.id (i + 1))))
  | Error _ as e -> e

(* Builds the header from the definitions the reader's first step passes. *)
let collector () =
  let stack = ref [] and top = ref [] and comment = ref None in
  let date = ref None and version = ref None and timescale = ref None and timezero = ref None in
  let add_scope s = match !stack with o :: _ -> o.o_children <- s :: o.o_children | [] -> top := s :: !top in
  let close () =
    match !stack with
    | o :: rest ->
        stack := rest;
        add_scope (close_scope o)
    | [] -> ()
  in
  let definition = function
    | Fst.Vcd.Date s -> date := text s
    | Version s -> version := text s
    | Timezero z -> timezero := Some z
    | Timescale s -> timescale := Some s
    | Scope { scope_type; name } ->
        stack := { st = scope_type_of_keyword scope_type; name; o_vars = []; o_children = [] } :: !stack
    | Upscope -> close ()
    | Comment s -> if !stack = [] then comment := Some (String.concat " " (words s))
    | Attr_begin _ | Attr_end -> ()
    | Var { var_type; size; id; name; _ } -> (
        let r, index =
          match words name with
          | r :: (_ :: _ as rest) -> (r, Some (String.concat " " rest))
          | [ r ] -> (r, None)
          | [] -> (name, None)
        in
        let v = { v_type = var_type_of_keyword var_type; size; id = Vcd_types.ID.of_string id; ref = r; index } in
        match !stack with o :: _ -> o.o_vars <- v :: o.o_vars | [] -> ())
    | End_definitions ->
        while !stack <> [] do
          close ()
        done
  in
  let header () =
    {
      version = !version;
      date = !date;
      timescale = !timescale;
      timezero = !timezero;
      comment = !comment;
      scopes = List.rev !top;
    }
  in
  (definition, header)

let events ?select ?(block = 0) t ~error ~ids ~definition =
  let resume = block > 0 in
  let q = Queue.create () in
  let push e = Queue.push e q in
  let id h = if h >= 1 && h <= Array.length ids then ids.(h - 1) else Vcd_types.ID.of_string (Fst.Vcd.id h) in
  (* A resumed stream has no [$dumpvars]: the frame's values go to [state] instead of the queue. *)
  let in_frame = ref false and state = ref Vcd.ID_map.empty in
  let change h v = if !in_frame then state := Vcd.ID_map.add (id h) v !state else push (Change (id h, v)) in
  (* [ignore_sink] keeps the build working when a field is added. *)
  let sink =
    {
      Fst.Vcd.ignore_sink with
      definition;
      time =
        (fun time ->
          in_frame := false;
          push (Timestamp (Vcd_types.Timestamp.of_int64 time)));
      frame = (fun _ -> in_frame := true);
      dump =
        (function
        | Fst.Vcd.Dumpvars -> push (DumpStart "dumpvars")
        | Dumpvars_end -> push DumpEnd
        | Dumpon ->
            push (DumpStart "dumpon");
            push DumpEnd
        | Dumpoff ->
            push (DumpStart "dumpoff");
            push DumpEnd);
      bit =
        (fun h c -> change h (try scalar_of_char c with Value.InvalidLogicChar _ -> Value.Other (String.make 1 c)));
      vector = (fun h s off len -> change h (binary_value (Bytes.sub_string s off len)));
      packed = Some (fun h b off width -> change h (packed_value b off width));
      port = (fun h s off len -> change h (Value.Port (Bytes.sub_string s off len)));
      real = (fun h d -> change h (Value.Real d));
      varlen = (fun h s off len -> change h (Value.String (Fst.Vcd.escape s off len)));
    }
    [@warning "-23"]
  in
  let select = Option.map (fun f h -> f (id h)) select in
  match Fst.Vcd.reader ~extensions:true ?select ~block t sink with
  | Error e -> raise (error e)
  | Ok r ->
      let rec next () =
        match Queue.take_opt q with
        | Some e -> Seq.Cons (e, next)
        | None -> (
            match Fst.Vcd.step r with
            | Ok true -> next ()
            | Ok false ->
                Fst.close t;
                Seq.Nil
            | Error e -> raise (error e))
      in
      (* The first step passes the definitions. A resumed stream's state is complete once the first block's frame
         and its first [time] have been delivered, so the second step is taken now. *)
      let step () = match Fst.Vcd.step r with Ok true -> () | Ok false -> Fst.close t | Error e -> raise (error e) in
      step ();
      if resume then step ();
      (!state, Seq.once next)

let parse_t ?select ~name opened =
  let error e = Vcd.Parse_error (Printf.sprintf "%s: %s" name (Fst.Error.to_string e)) in
  match opened with
  | Error e when Fst.Error.kind e = Fst.Error.Io -> raise (Sys_error (Fst.Error.message e))
  | Error e -> raise (error e)
  | Ok t -> (
      let error e =
        Fst.close t;
        error e
      in
      match ids_of t with
      | Error e -> raise (error e)
      | Ok ids ->
          let definition, header = collector () in
          let _, simulation = events ?select t ~error ~ids ~definition in
          (t, { Vcd.header = header (); simulation }))

let parse ?select ~name opened = snd (parse_t ?select ~name opened)
let parse_string ?select s = parse ?select ~name:"<string>" (Fst.of_string s)
let parse_file ?select path = parse ?select ~name:path (Fst.open_file path)

let open_string ?select s =
  match detect_string s with `Fst -> parse_string ?select s | `Vcd -> Vcd.parse_string ?select s

let open_file ?select path =
  match detect_file path with `Fst -> parse_file ?select path | `Vcd -> Vcd.parse_file ?select path

let with_file ?select path f =
  match detect_file path with
  | `Vcd -> Vcd.with_file ?select path f
  | `Fst ->
      let t, r = parse_t ?select ~name:path (Fst.open_file path) in
      Fun.protect ~finally:(fun () -> Fst.close t) (fun () -> f r)

(* An FST opened for [fn]; raises as [parse] does, and [Invalid_argument] for a VCD. *)
let with_fst ~fn path f =
  if detect_file path = `Vcd then invalid_arg (fn ^ ": not an FST file");
  let error e = Vcd.Parse_error (Printf.sprintf "%s: %s" path (Fst.Error.to_string e)) in
  match Fst.open_file path with
  | Error e when Fst.Error.kind e = Fst.Error.Io -> raise (Sys_error (Fst.Error.message e))
  | Error e -> raise (error e)
  | Ok t ->
      let error e =
        Fst.close t;
        error e
      in
      f t error

let blocks path =
  with_fst ~fn:"Vcd_fst.blocks" path (fun t error ->
      match Fst.blocks t with
      | Error e -> raise (error e)
      | Ok bs ->
          Fst.close t;
          Array.map
            (fun (b : Fst.block) ->
              (Vcd_types.Timestamp.of_int64 b.start_time, Vcd_types.Timestamp.of_int64 b.end_time))
            bs)

let open_at ?select path ~time =
  with_fst ~fn:"Vcd_fst.open_at" path (fun t error ->
      match ids_of t with
      | Error e -> raise (error e)
      | Ok ids ->
          let time = Vcd_types.Timestamp.to_int64 time in
          let block =
            match Fst.blocks t with
            | Error e -> raise (error e)
            | Ok [||] -> 0
            | Ok bs -> (
                let k = ref 0 in
                Array.iteri (fun i (b : Fst.block) -> if Int64.compare b.start_time time <= 0 then k := i) bs;
                match Fst.aligned_block t !k with Ok j -> j | Error e -> raise (error e))
          in
          let definition, header = collector () in
          let state, simulation = events ?select ~block t ~error ~ids ~definition in
          (state, { Vcd.header = header (); simulation }))
