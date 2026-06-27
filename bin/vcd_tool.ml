module Stateful = Vcd.Stateful

let parse_pattern_or_exit r s =
  match Filter_parser.parse s with
  | Error msg ->
      Printf.eprintf "error: invalid signal pattern %S: %s\n" s msg;
      exit 1
  | Ok pat -> r := (s, pat) :: !r

let pp_changes resolver strip_map filter ev =
  Vcd.ID_map.iter
    (fun id v ->
      match Vcd.Resolver.find resolver id with
      | None -> ()
      | Some entry ->
          let size = Vcd.Resolver.entry_size entry in
          Vcd.Ref_set.iter
            (fun r ->
              let display = match strip_map with None -> r | Some f -> f r in
              Format.printf "  %s=%s\n" (Vcd_types.Reference.to_string display) (Vcd_types.Value.to_string_hex size v))
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
                let display = match strip_map with None -> r | Some f -> f r in
                (Vcd_types.Reference.to_string display, Vcd_types.Value.to_string_hex size v) :: acc)
              (Vcd_util.selected_refs resolver show_filter id)
              acc
        | _ -> acc)
      show_ids []
    |> List.sort_uniq compare
  in
  let print_show indent state =
    List.iter (fun (path, v) -> Format.printf "%s%s=%s\n" indent path v) (show_lines state)
  in
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
            match Vcd.ID_map.find_opt id ev.Stateful.changes with
            | Some v when Vcd_types.Value.get_int ~default:min_int v = expected ->
                Vcd.Ref_set.fold
                  (fun r acc ->
                    let display = match strip_map with None -> r | Some f -> f r in
                    (Vcd_types.Reference.to_string display, Vcd_types.Value.to_string_hex size v) :: acc)
                  refs []
            | _ -> [])
          triggers
      in
      if fired <> [] then begin
        List.iter (fun (path, v) -> Format.printf "@%a %s=%s\n" Vcd_types.Timestamp.pp ev.Stateful.time path v) fired;
        match !recent with
        | [] -> ()
        | (_, state) :: older ->
            (* Newest snapshot is the trigger timestamp; [older] are the previous
               change-timestamps, labelled -1, -2, ... back. *)
            print_show "    " state;
            List.iteri
              (fun i (t, state) ->
                Format.printf "  -%d @%a\n" (i + 1) Vcd_types.Timestamp.pp t;
                print_show "    " state)
              older
      end)
    events

let () =
  let ranges = ref [] in
  let patterns = ref [] in
  let regexes = ref [] in
  let strip = ref false in
  let when_specs = ref [] in
  let show_patterns = ref [] in
  let lookback = ref 0 in
  let files = ref [] in
  let spec =
    [
      ( "--range",
        Arg.String
          (fun s ->
            match Vcd_util.parse_range s with
            | exception Failure msg ->
                Printf.eprintf "error: invalid --range %S: %s\n" s msg;
                exit 1
            | r -> ranges := r :: !ranges),
        "SPEC  Restrict to a time range, e.g. \"...1000\" or \"0-500\" or \"2000-...\"; may be repeated" );
      ( "--signal",
        Arg.String (parse_pattern_or_exit patterns),
        "PATTERN  Show only signals matching this DSL pattern (may be repeated)" );
      ( "--signal-re",
        Arg.String
          (fun s ->
            match Re.Pcre.re s |> Re.compile with
            | exception exn ->
                Printf.eprintf "error: invalid regex %S: %s\n" s (Printexc.to_string exn);
                exit 1
            | re -> regexes := (s, re) :: !regexes),
        "REGEX  Show only signals whose full name matches this PCRE regex (may be repeated)" );
      ("--strip", Arg.Set strip, " Strip the longest common scope prefix from signal names");
      ( "--when",
        Arg.String
          (fun s ->
            match String.rindex_opt s '=' with
            | None ->
                Printf.eprintf "error: --when %S must be of the form PATTERN=VALUE\n" s;
                exit 1
            | Some i -> (
                let pat_s = String.sub s 0 i and value_s = String.sub s (i + 1) (String.length s - i - 1) in
                match Filter_parser.parse pat_s with
                | Error msg ->
                    Printf.eprintf "error: invalid signal pattern %S: %s\n" pat_s msg;
                    exit 1
                | Ok pat -> (
                    match int_of_string_opt (String.trim value_s) with
                    | None ->
                        Printf.eprintf "error: --when %S has non-integer value %S\n" s value_s;
                        exit 1
                    | Some v -> when_specs := (s, pat, v) :: !when_specs))),
        "PATTERN=VALUE  Trigger on each timestamp where a matching signal takes VALUE (may be repeated)" );
      ( "--show",
        Arg.String
          (fun s ->
            String.split_on_char ',' s
            |> List.iter (fun p ->
                let p = String.trim p in
                if p <> "" then parse_pattern_or_exit show_patterns p)),
        "PATTERNS  Comma-separated signals to print at each --when trigger (may be repeated)" );
      ( "--lookback",
        Arg.Int
          (fun n ->
            if n < 0 then (
              Printf.eprintf "error: --lookback must be >= 0\n";
              exit 1)
            else lookback := n),
        "N  Also print --show state at the previous N change-timestamps (default 0)" );
    ]
  in
  let usage =
    "usage: vcd_tool [--range SPEC]... [--signal PATTERN]... [--signal-re REGEX]... [--strip] [--when PATTERN=VALUE \
     --show PATTERNS [--lookback N]]... <file.vcd>..."
  in
  Arg.parse spec (fun f -> files := f :: !files) usage;
  let files = List.rev !files in
  let ranges = List.rev !ranges in
  let patterns = List.rev !patterns in
  let regexes = List.rev !regexes in
  let when_specs = List.rev !when_specs in
  let show_patterns = List.rev !show_patterns in
  let lookback = !lookback in
  if files = [] then (
    Arg.usage spec usage;
    exit 1);
  List.iter
    (fun path ->
      let result =
        match Vcd.parse_file path with
        | r -> r
        | exception Vcd.Parse_error msg ->
            Printf.eprintf "parse error: %s\n" msg;
            exit 1
        | exception Sys_error msg ->
            Printf.eprintf "%s\n" msg;
            exit 1
        | exception Failure msg ->
            Printf.eprintf "error: %s\n" msg;
            exit 1
      in
      let resolver = Vcd.Resolver.make result.header in
      if when_specs <> [] then begin
        let show_filter = Vcd_util.build_filter ~pattern_label:"--show" resolver show_patterns [] in
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
        let events = Stateful.stream ~tracked ~ranges result.simulation in
        correlate resolver strip_map ~when_specs ~show_filter ~lookback events
      end
      else begin
        let filter = Vcd_util.build_filter resolver patterns regexes in
        let strip_map = if !strip then Vcd_util.build_strip_map resolver filter else None in
        let events =
          match filter with
          | None -> Stateful.stream ~ranges result.simulation
          | Some f -> Stateful.stream ~reported:(Vcd_util.filter_ids f) ~ranges result.simulation
        in
        Seq.iter
          (fun ev ->
            Format.printf "#%a\n" Vcd_types.Timestamp.pp ev.Stateful.time;
            pp_changes resolver strip_map filter ev)
          events
      end)
    files
