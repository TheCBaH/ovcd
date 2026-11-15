open Vcd_ast
module ID_set = Vcd_util.ID_set

let opt f = function None -> "(none)" | Some x -> f x

let rec pp_scope indent s =
  Printf.printf "%sscope %s %s\n" indent (Vcd.string_of_scope_type s.s_type) s.s_name;
  List.iter
    (fun v ->
      Printf.printf "%s  var %s %d %s%s\n" indent (Vcd.string_of_var_type v.v_type) v.size v.ref
        (match v.index with None -> "" | Some i -> " " ^ i))
    s.vars;
  List.iter (pp_scope (indent ^ "  ")) s.children

let count_scopes_and_vars scopes =
  let rec walk (ns, nv) s = List.fold_left walk (ns + 1, nv + List.length s.vars) s.children in
  List.fold_left walk (0, 0) scopes

let show_id filter id = Vcd_util.mem_filter filter id

type options = {
  sel : Tool_cli.selection;
  summary : bool;
  bench_runs : int;
  resolve : bool;
  strip : bool;
  list_signals : (string * Filter_ast.pattern) list;
  jsonl : bool;
}

let list_signals o path =
  (* Header-only query: print every matched signal's path, ID code and width.
     Reuses the filter machinery so unmatched patterns warn on stderr and only
     the references that actually matched are listed (not unselected aliases). *)
  Tool_cli.with_parsed path @@ fun parsed ->
  let h = parsed.header in
  let resolver = Vcd.Resolver.make h in
  let filter, unmatched = Vcd_util.build_filter resolver o.list_signals [] in
  Tool_cli.warn_unmatched ~label:"--list-signals" unmatched;
  let strip_map = if o.strip then Vcd_util.build_strip_map resolver filter else None in
  if o.jsonl then begin
    let wanted (s : Vcd_signals.t) =
      let r = Vcd_types.Reference.of_list s.parts in
      match Vcd.Resolver.find_id resolver r with
      | None -> false
      | Some id -> Vcd.Ref_set.mem r (Vcd_util.selected_refs resolver filter id)
    in
    Vcd_signals.write stdout (Seq.filter wanted (Vcd_signals.of_header h))
  end
  else begin
    let rows =
      Vcd.Resolver.fold
        (fun entry acc ->
          let id = Vcd.Resolver.entry_id entry in
          let size = Vcd.Resolver.entry_size entry in
          Vcd.Ref_set.fold
            (fun r acc ->
              (Vcd_types.Reference.to_string (Tool_cli.display strip_map r), Vcd_types.ID.to_string id, size) :: acc)
            (Vcd_util.selected_refs resolver filter id)
            acc)
        resolver []
    in
    List.iter (fun (path, id, size) -> Printf.printf "%s\t%s\t%d\n" path id size) (List.sort_uniq compare rows)
  end

(* The filter exists only once the header is parsed; the first pull of the simulation reads it. *)
let bench o ~select ~filter_ref path =
  for _ = 1 to o.bench_runs do
    Tool_cli.with_parsed ?select path @@ fun r ->
    if o.resolve then begin
      let resolver = Vcd.Resolver.make r.header in
      let filter, unmatched = Vcd_util.build_filter resolver !(o.sel.patterns) !(o.sel.regexes) in
      Tool_cli.warn_unmatched ~label:"--signal" unmatched;
      filter_ref := filter;
      Seq.iter (function Change (id, _) when show_id filter id -> () | _ -> ()) r.simulation
    end
    else Seq.iter ignore r.simulation
  done

let resolved o ~select ~filter_ref path =
  let ranges = !(o.sel.ranges) in
  Tool_cli.with_parsed ?select path @@ fun result ->
  let resolver = Vcd.Resolver.make result.header in
  let filter, unmatched = Vcd_util.build_filter resolver !(o.sel.patterns) !(o.sel.regexes) in
  Tool_cli.warn_unmatched ~label:"--signal" unmatched;
  filter_ref := filter;
  let strip_map = if o.strip then Vcd_util.build_strip_map resolver filter else None in
  let names_of id =
    match Vcd.Resolver.find resolver id with
    | None -> ([ Vcd_types.ID.to_string id ], 0)
    | Some entry ->
        let size = Vcd.Resolver.entry_size entry in
        let names =
          Vcd.Ref_set.fold
            (fun r acc -> Vcd_types.Reference.to_string (Tool_cli.display strip_map r) :: acc)
            (Vcd_util.selected_refs resolver filter id)
            []
        in
        (names, size)
  in
  let cache = Hashtbl.create 256 in
  let resolve id =
    match Hashtbl.find_opt cache id with
    | Some r -> r
    | None ->
        let r = names_of id in
        Hashtbl.add cache id r;
        r
  in
  let cur_in_range = ref (ranges = []) in
  let pending_ts = ref None in
  let flush () =
    match !pending_ts with
    | None -> ()
    | Some t ->
        Tool_out.text ("#" ^ Tool_out.ts t);
        pending_ts := None
  in
  try
    Seq.iter
      (function
        | Timestamp t when Vcd_util.past_all_ranges ranges t -> raise Exit
        | Timestamp t ->
            let in_r = Vcd_util.in_ranges ranges t in
            cur_in_range := in_r;
            pending_ts := if in_r then Some t else None
        | Change (id, v) when !cur_in_range && show_id filter id ->
            flush ();
            let names, size = resolve id in
            let hex = Vcd_types.Value.to_string_hex size v in
            List.iter (fun name -> Tool_out.assignment "  " name hex) names
        | Change _ -> ()
        | DumpStart k when !cur_in_range ->
            flush ();
            Tool_out.text ("$" ^ k)
        | DumpStart _ -> ()
        | DumpEnd when !cur_in_range -> Tool_out.text "$end"
        | DumpEnd -> ()
        | SimComment c when !cur_in_range ->
            flush ();
            Tool_out.text ("//" ^ c)
        | SimComment _ -> ())
      result.simulation
  with Exit -> ()

let overview o path =
  let ranges = !(o.sel.ranges) in
  Tool_cli.with_parsed path @@ fun result ->
  let h = result.header in
  Printf.printf "file:      %s\n" (Filename.basename path);
  Printf.printf "date:      %s\n" (opt Fun.id h.date);
  Printf.printf "version:   %s\n" (opt Fun.id h.version);
  Printf.printf "timescale: %s\n" (opt Fun.id h.timescale);
  Option.iter (Printf.printf "timezero:  %Ld\n") h.timezero;
  if o.summary then begin
    let ns, nv = count_scopes_and_vars h.scopes in
    Printf.printf "scopes:    %d  vars: %d\n" ns nv
  end
  else List.iter (pp_scope "") h.scopes;
  let cur_in_range = ref (ranges = []) in
  let timestamps = ref 0 and changes = ref 0 and dumps = ref 0 and comments = ref 0 in
  (try
     Seq.iter
       (function
         | Timestamp t when Vcd_util.past_all_ranges ranges t -> raise Exit
         | Timestamp t ->
             let in_r = Vcd_util.in_ranges ranges t in
             cur_in_range := in_r;
             if in_r then incr timestamps
         | Change _ -> if !cur_in_range then incr changes
         | DumpStart _ -> if !cur_in_range then incr dumps
         | DumpEnd -> ()
         | SimComment _ -> if !cur_in_range then incr comments)
       result.simulation
   with Exit -> ());
  Printf.printf "events:    %d (timestamps: %d, changes: %d, dumps: %d, comments: %d)\n"
    (!timestamps + !changes + !dumps + !comments)
    !timestamps !changes !dumps !comments

let main () =
  let sel = Tool_cli.selection () in
  let summary = ref false and bench_runs = ref 0 and resolve = ref false and strip = ref false in
  let list_signals_pats = ref [] and format = ref None in
  let spec =
    [
      ("--summary", Arg.Set summary, " Print scope/var counts instead of tree");
      ( "--bench",
        Arg.Int (fun n -> if n > 0 then bench_runs := n else Tool_cli.fail "error: --bench requires a positive integer"),
        "N  Parse N times for benchmarking" );
      ("--resolve", Arg.Set resolve, " Resolve IDs to hierarchical signal names in simulation output");
      ( "--strip",
        Arg.Set strip,
        " Strip the longest common scope prefix from all matched signal names (requires --resolve)" );
    ]
    @ Tool_cli.selection_specs sel
        ~signal_doc:
          "PATTERN  Show only matching signals (exact name or DSL pattern; may be repeated; implies --resolve)"
        ~regex_doc:
          "REGEX  Show only signals whose full name matches this PCRE regex (may be repeated; implies --resolve)"
    @ [
        ( "--list-signals",
          Arg.String (Tool_cli.add_pattern list_signals_pats),
          "PATTERN  List matching signals as 'path<TAB>id<TAB>width' and exit (may be repeated)" );
        ( "--format",
          Arg.Symbol ([ "tsv"; "jsonl" ], fun f -> format := Some f),
          " Output of --list-signals: tsv (default), or jsonl in declaration order with var type and scope kinds" );
      ]
  in
  let usage =
    "usage: vcd_dump [--summary] [--bench N] [--resolve] [--strip] [--range SPEC]... [--signal PATTERN]... \
     [--signal-re REGEX]... [--list-signals PATTERN]... [--format tsv|jsonl] <file.vcd|file.fst>"
  in
  let files = Tool_cli.parse_args spec usage in
  let selecting = !(sel.patterns) <> [] || !(sel.regexes) <> [] in
  let listing = !list_signals_pats <> [] in
  let jsonl = !format = Some "jsonl" in
  (* Selecting signals by name is only meaningful in the resolved output, so --signal / --signal-re imply
     --resolve. *)
  let resolve = !resolve || selecting in
  Tool_cli.reject (!format <> None && not listing) "--format applies only to --list-signals";
  Tool_cli.reject (!summary && listing) "--summary cannot be combined with --list-signals";
  Tool_cli.reject (!summary && resolve) "--summary cannot be combined with --resolve, --signal or --signal-re";
  Tool_cli.reject (selecting && listing)
    "--signal and --signal-re cannot be combined with --list-signals (give the patterns to --list-signals)";
  Tool_cli.reject (!strip && jsonl) "--strip cannot be combined with --format jsonl";
  Tool_cli.reject (!strip && not (resolve || listing)) "--strip requires --resolve or --list-signals";
  let o =
    {
      sel =
        {
          Tool_cli.ranges = ref (List.rev !(sel.ranges));
          patterns = ref (List.rev !(sel.patterns));
          regexes = ref (List.rev !(sel.regexes));
        };
      summary = !summary;
      bench_runs = !bench_runs;
      resolve;
      strip = !strip;
      list_signals = List.rev !list_signals_pats;
      jsonl;
    }
  in
  let filter_ref = ref None in
  let select = if o.resolve && selecting then Some (fun id -> Vcd_util.mem_filter !filter_ref id) else None in
  List.iter
    (fun path ->
      if listing then list_signals o path
      else if o.bench_runs > 0 then bench o ~select ~filter_ref path
      else if o.resolve then resolved o ~select ~filter_ref path
      else overview o path)
    files

(* Value changes are parsed lazily, so their errors surface while iterating. *)
let () = Tool_cli.run main
