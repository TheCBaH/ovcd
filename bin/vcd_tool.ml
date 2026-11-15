module Stateful = Vcd.Stateful

let pp_changes resolver strip_map filter ev =
  Vcd.ID_map.iter
    (fun id v ->
      match Vcd.Resolver.find resolver id with
      | None -> ()
      | Some entry ->
          let size = Vcd.Resolver.entry_size entry in
          Vcd.Ref_set.iter
            (fun r ->
              let display = Tool_cli.display strip_map r in
              Tool_out.assignment "  " (Vcd_types.Reference.to_string display) (Vcd_types.Value.to_string_hex size v))
            (Vcd_util.selected_refs resolver filter id))
    ev.Stateful.changes

(* Edge-correlation mode (--when/--show/--lookback): for every timestamp where a
   --when signal takes the requested value, print the --show signals at that
   timestamp and at the previous [lookback] change-timestamps. Built on
   Stateful.stream; each snapshot reflects values as of that timestamp inclusive
   (event.state is "as of the previous timestep", so the current change is
   overlaid before snapshotting). *)
let correlate resolver strip_map ~when_specs ~show_filter ~lookback events =
  let show_ids = match show_filter with Some f -> Vcd_util.filter_ids f | None -> Vcd.ID_set.empty in
  (* (trigger_id, expected_value, width, matched references) for display. *)
  let triggers =
    List.concat_map
      (fun (raw, pat, expected) ->
        match Vcd.Resolver.find_all pat resolver with
        | [] ->
            Printf.eprintf "warning: --when %S matched no signals\n" raw;
            []
        | entries ->
            List.map
              (fun e ->
                let refs = Vcd.Ref_set.filter (Filter_matcher.matches pat) (Vcd.Resolver.entry_references e) in
                (Vcd.Resolver.entry_id e, expected, Vcd.Resolver.entry_size e, refs))
              entries)
      when_specs
  in
  (* Show-signal lines for one state snapshot, sorted by path for stable output. *)
  let show_lines state =
    Vcd.ID_set.fold
      (fun id acc ->
        match (Vcd.Resolver.find resolver id, Vcd.ID_map.find_opt id state) with
        | Some entry, Some v ->
            let size = Vcd.Resolver.entry_size entry in
            Vcd.Ref_set.fold
              (fun r acc ->
                let display = Tool_cli.display strip_map r in
                (Vcd_types.Reference.to_string display, Vcd_types.Value.to_string_hex size v) :: acc)
              (Vcd_util.selected_refs resolver show_filter id)
              acc
        | _ -> acc)
      show_ids []
    |> List.sort_uniq compare
  in
  let print_show indent state = List.iter (fun (path, v) -> Tool_out.assignment indent path v) (show_lines state) in
  (* Newest-first ring of the last (lookback + 1) snapshots. *)
  let recent = ref [] in
  let rec take n = function [] -> [] | _ when n = 0 -> [] | x :: xs -> x :: take (n - 1) xs in
  Seq.iter
    (fun ev ->
      (* event.state is the accumulated state as of the previous timestep (and
         includes values set inside $dumpvars, which never appear in changes);
         overlay this timestep's changes to get the state as of now, inclusive. *)
      let cur = Vcd.ID_map.fold Vcd.ID_map.add ev.Stateful.changes ev.Stateful.state in
      recent := take (lookback + 1) ((ev.Stateful.time, cur) :: !recent);
      let fired =
        List.concat_map
          (fun (id, expected, size, refs) ->
            match (ev.Stateful.kind, Vcd.ID_map.find_opt id ev.Stateful.changes) with
            | `Delta, Some v when Vcd_types.Value.get_int ~default:min_int v = expected ->
                Vcd.Ref_set.fold
                  (fun r acc ->
                    let display = Tool_cli.display strip_map r in
                    (Vcd_types.Reference.to_string display, Vcd_types.Value.to_string_hex size v) :: acc)
                  refs []
            | _ -> [])
          triggers
      in
      if fired <> [] then begin
        List.iter (fun (path, v) -> Tool_out.assignment ("@" ^ Tool_out.ts ev.Stateful.time ^ " ") path v) fired;
        match !recent with
        | [] -> ()
        | (_, state) :: older ->
            (* Newest snapshot is the trigger timestamp; [older] are the previous
               change-timestamps, labelled -1, -2, ... back. *)
            print_show "    " state;
            List.iteri
              (fun i (t, state) ->
                Tool_out.text (Printf.sprintf "  -%d @%s" (i + 1) (Tool_out.ts t));
                print_show "    " state)
              older
      end)
    events

let main () =
  let sel = Tool_cli.selection () in
  let strip = ref false in
  let when_specs = ref [] in
  let show_patterns = ref [] in
  let lookback = ref None in
  let spec =
    Tool_cli.selection_specs sel ~signal_doc:"PATTERN  Show only signals matching this DSL pattern (may be repeated)"
      ~regex_doc:"REGEX  Show only signals whose full name matches this PCRE regex (may be repeated)"
    @ [
        ("--strip", Arg.Set strip, " Strip the longest common scope prefix from signal names");
        ( "--when",
          Arg.String
            (fun s ->
              match String.rindex_opt s '=' with
              | None -> Tool_cli.fail "error: --when %S must be of the form PATTERN=VALUE" s
              | Some i -> (
                  let pat_s = String.sub s 0 i and value_s = String.sub s (i + 1) (String.length s - i - 1) in
                  match Filter_parser.parse pat_s with
                  | Error msg -> Tool_cli.fail "error: invalid signal pattern %S: %s" pat_s msg
                  | Ok pat -> (
                      match int_of_string_opt (String.trim value_s) with
                      | None -> Tool_cli.fail "error: --when %S has non-integer value %S" s value_s
                      | Some v -> when_specs := (s, pat, v) :: !when_specs))),
          "PATTERN=VALUE  Trigger on each timestamp where a matching signal takes VALUE (may be repeated)" );
        ( "--show",
          Arg.String
            (fun s ->
              String.split_on_char ',' s
              |> List.iter (fun p ->
                  let p = String.trim p in
                  if p <> "" then Tool_cli.add_pattern show_patterns p)),
          "PATTERNS  Comma-separated signals to print at each --when trigger (may be repeated)" );
        ( "--lookback",
          Arg.Int (fun n -> if n < 0 then Tool_cli.fail "error: --lookback must be >= 0" else lookback := Some n),
          "N  Also print --show state at the previous N change-timestamps (default 0)" );
      ]
  in
  let usage =
    "usage: vcd_tool [--range SPEC]... [--signal PATTERN]... [--signal-re REGEX]... [--strip] [--when PATTERN=VALUE \
     --show PATTERNS [--lookback N]]... <file.vcd|file.fst>..."
  in
  let files = Tool_cli.parse_args spec usage in
  let ranges = List.rev !(sel.ranges) in
  let patterns = List.rev !(sel.patterns) in
  let regexes = List.rev !(sel.regexes) in
  let when_specs = List.rev !when_specs in
  let show_patterns = List.rev !show_patterns in
  let selecting = patterns <> [] || regexes <> [] in
  Tool_cli.reject (selecting && when_specs <> []) "--signal and --signal-re cannot be combined with --when (use --show)";
  Tool_cli.reject (when_specs = [] && (show_patterns <> [] || !lookback <> None)) "--show and --lookback require --when";
  let lookback = Option.value ~default:0 !lookback in
  (* The IDs to read are known only once the header is parsed; the first pull of the simulation asks. *)
  let wanted = ref None in
  let select =
    if when_specs <> [] || selecting then
      Some (fun id -> match !wanted with None -> true | Some s -> Vcd.ID_set.mem id s)
    else None
  in
  List.iter
    (fun path ->
      Tool_cli.with_parsed ?select path @@ fun result ->
      let resolver = Vcd.Resolver.make result.header in
      if when_specs <> [] then begin
        let show_filter, unmatched = Vcd_util.build_filter resolver show_patterns [] in
        Tool_cli.warn_unmatched ~label:"--show" unmatched;
        let strip_map = if !strip then Vcd_util.build_strip_map resolver show_filter else None in
        let trigger_ids =
          List.fold_left
            (fun s (_, pat, _) ->
              List.fold_left
                (fun s e -> Vcd.ID_set.add (Vcd.Resolver.entry_id e) s)
                s (Vcd.Resolver.find_all pat resolver))
            Vcd.ID_set.empty when_specs
        in
        let show_ids = match show_filter with Some f -> Vcd_util.filter_ids f | None -> Vcd.ID_set.empty in
        let tracked = Vcd.ID_set.union trigger_ids show_ids in
        wanted := Some tracked;
        let events = Stateful.stream ~tracked ~ranges result.simulation in
        correlate resolver strip_map ~when_specs ~show_filter ~lookback events
      end
      else begin
        let filter, unmatched = Vcd_util.build_filter resolver patterns regexes in
        Tool_cli.warn_unmatched ~label:"--signal" unmatched;
        let strip_map = if !strip then Vcd_util.build_strip_map resolver filter else None in
        wanted := Option.map Vcd_util.filter_ids filter;
        let events =
          match filter with
          | None -> Stateful.stream ~ranges result.simulation
          | Some f -> Stateful.stream ~reported:(Vcd_util.filter_ids f) ~ranges result.simulation
        in
        Seq.iter
          (fun ev ->
            Tool_out.text ("#" ^ Tool_out.ts ev.Stateful.time);
            pp_changes resolver strip_map filter ev)
          events
      end)
    files

(* Value changes are parsed lazily, so their errors surface while iterating. *)
let () = Tool_cli.run main
