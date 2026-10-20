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

(* The header, and for each handle (index [handle - 1]) its ID and whether it is a port. *)
let definitions t =
  let h = Fst.header t in
  let stack = ref [] and top = ref [] and comment = ref None in
  let ids = ref [] and ports = ref [] in
  let add_scope s = match !stack with o :: _ -> o.o_children <- s :: o.o_children | [] -> top := s :: !top in
  let entry () = function
    | Fst.Hier.Scope { scope_type; name; _ } ->
        let kw = Option.value (Fst.Hier.scope_type_name scope_type) ~default:"module" in
        stack := { st = scope_type_of_keyword kw; name; o_vars = []; o_children = [] } :: !stack
    | Upscope -> (
        match !stack with
        | o :: rest ->
            stack := rest;
            add_scope (close_scope o)
        | [] -> ())
    | Attr_begin { attr_type = 0; subtype = 0; name; _ } when !stack = [] ->
        comment := Some (String.concat " " (words (if name = "" then "\"\"" else name)))
    | Attr_begin _ | Attr_end -> ()
    | Var { var_type; name; length; handle; alias; _ } -> (
        let id = Vcd_types.ID.of_string (Fst.Vcd.id handle) in
        let kw = Fst.Hier.var_type_name var_type in
        if not alias then begin
          ids := id :: !ids;
          ports := (kw = Some "port") :: !ports
        end;
        let v_type = match kw with Some kw -> var_type_of_keyword kw | None -> Other_type (string_of_int var_type) in
        let r, index =
          match words name with
          | r :: (_ :: _ as rest) -> (r, Some (String.concat " " rest))
          | [ r ] -> (r, None)
          | [] -> (name, None)
        in
        let v = { v_type; size = Fst.Vcd.var_length ~var_type ~length; id; ref = r; index } in
        match !stack with o :: _ -> o.o_vars <- v :: o.o_vars | [] -> ())
  in
  match Fst.fold_hierarchy t ~init:() ~f:entry with
  | Error _ as e -> e
  | Ok () ->
      let rec unwind () =
        match !stack with
        | o :: rest ->
            stack := rest;
            add_scope (close_scope o);
            unwind ()
        | [] -> ()
      in
      unwind ();
      let header =
        {
          version = text h.version;
          date = text h.date;
          timescale = Some (Fst.Vcd.timescale h.timescale);
          timezero = (if Int64.equal h.timezero 0L then None else Some h.timezero);
          comment = !comment;
          scopes = List.rev !top;
        }
      in
      let arr l = Array.of_list (List.rev l) in
      Ok (header, arr !ids, arr !ports)

(* Events in the order, and with the [$dumpvars] and [$dumpon]/[$dumpoff] placement, of fst2vcd's output. *)
let events ?select t ~error ~ids ~ports =
  let q = Queue.create () in
  let push e = Queue.push e q in
  let id h = if h >= 1 && h <= Array.length ids then ids.(h - 1) else Vcd_types.ID.of_string (Fst.Vcd.id h) in
  let port h = h >= 1 && h <= Array.length ports && ports.(h - 1) in
  let dumpvars = ref 0 in
  let blackouts = Array.of_list (Fst.blackouts t) and cur = ref 0 in
  let new_time time =
    if !dumpvars = 1 then begin
      push DumpEnd;
      dumpvars := 2
    end;
    push (Timestamp (Vcd_types.Timestamp.of_int64 time));
    if !dumpvars = 0 then begin
      push (DumpStart "dumpvars");
      dumpvars := 1
    end
  in
  let blackout time =
    if !cur < Array.length blackouts then begin
      let bt, on = blackouts.(!cur) in
      if Int64.equal bt time then begin
        push (DumpStart (if on then "dumpon" else "dumpoff"));
        push DumpEnd;
        incr cur
      end
    end
  in
  let sink =
    {
      Fst.time =
        (fun time ->
          new_time time;
          blackout time);
      frame =
        (fun time ->
          if not (Int64.equal time 0L) then new_time time;
          blackout time);
      bit = (fun h c -> push (Change (id h, scalar_of_char c)));
      vector =
        (fun h s off len ->
          let v = Bytes.sub_string s off len in
          push (Change (id h, if port h then Value.Port v else binary_value v)));
      real = (fun h d -> push (Change (id h, Value.Real d)));
      varlen = (fun h s off len -> push (Change (id h, Value.String (Fst.Vcd.escape s off len))));
    }
  in
  let packed h b off width =
    let v =
      if port h then
        Value.Port
          (String.init width (fun j ->
               if (Char.code (Bytes.get b (off + (j lsr 3))) lsr (7 - (j land 7))) land 1 = 1 then '1' else '0'))
      else packed_value b off width
    in
    push (Change (id h, v))
  in
  match Fst.reader ?select:(Option.map (fun f h -> f (id h)) select) ~packed t sink with
  | Error e -> raise (error e)
  | Ok r ->
      let rec next () =
        match Queue.take_opt q with
        | Some e -> Seq.Cons (e, next)
        | None -> (
            match Fst.step r with
            | Ok true -> next ()
            | Ok false ->
                Fst.close t;
                Seq.Nil
            | Error e -> raise (error e))
      in
      next

let parse ?select ~name opened =
  let error e = Vcd.Parse_error (Printf.sprintf "%s: %s" name (Fst.Error.to_string e)) in
  match opened with
  | Error e when Fst.Error.kind e = Fst.Error.Io -> raise (Sys_error (Fst.Error.message e))
  | Error e -> raise (error e)
  | Ok t -> (
      let error e =
        Fst.close t;
        error e
      in
      match definitions t with
      | Error e -> raise (error e)
      | Ok (header, ids, ports) -> { Vcd.header; simulation = events ?select t ~error ~ids ~ports })

let parse_string ?select s = parse ?select ~name:"<string>" (Fst.of_string s)
let parse_file ?select path = parse ?select ~name:path (Fst.open_file path)

let open_string ?select s =
  match detect_string s with `Fst -> parse_string ?select s | `Vcd -> Vcd.parse_string ?select s

let open_file ?select path =
  match detect_file path with `Fst -> parse_file ?select path | `Vcd -> Vcd.parse_file ?select path
