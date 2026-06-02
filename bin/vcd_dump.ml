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

let show_id filter id = match filter with None -> true | Some s -> ID_set.mem id s

let () =
  let summary = ref false in
  let bench_runs = ref 0 in
  let resolve = ref false in
  let strip = ref false in
  let ranges = ref [] in
  let patterns = ref [] in
  let regexes = ref [] in
  let files = ref [] in
  let spec =
    [
      ("--summary", Arg.Set summary, " Print scope/var counts instead of tree");
      ( "--bench",
        Arg.Int
          (fun n ->
            if n > 0 then bench_runs := n
            else (
              Printf.eprintf "error: --bench requires a positive integer\n";
              exit 1)),
        "N  Parse N times for benchmarking" );
      ("--resolve", Arg.Set resolve, " Resolve IDs to hierarchical signal names in simulation output");
      ( "--strip",
        Arg.Set strip,
        " Strip the longest common scope prefix from all matched signal names (requires --resolve)" );
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
        Arg.String
          (fun s ->
            match Filter_parser.parse s with
            | Error msg ->
                Printf.eprintf "error: invalid signal pattern %S: %s\n" s msg;
                exit 1
            | Ok pat -> patterns := (s, pat) :: !patterns),
        "PATTERN  Show only matching signals (exact name or DSL pattern; may be repeated; requires --resolve)" );
      ( "--signal-re",
        Arg.String
          (fun s ->
            match Re.Pcre.re s |> Re.compile with
            | exception exn ->
                Printf.eprintf "error: invalid regex %S: %s\n" s (Printexc.to_string exn);
                exit 1
            | re -> regexes := (s, re) :: !regexes),
        "REGEX  Show only signals whose full name matches this PCRE regex (may be repeated; requires --resolve)" );
    ]
  in
  let usage =
    "usage: vcd_dump [--summary] [--bench N] [--resolve] [--strip] [--range SPEC]... [--signal PATTERN]... \
     [--signal-re REGEX]... <file.vcd>"
  in
  Arg.parse spec (fun f -> files := f :: !files) usage;
  let files = List.rev !files in
  let ranges = List.rev !ranges in
  let patterns = List.rev !patterns in
  let regexes = List.rev !regexes in
  if files = [] then (
    Arg.usage spec usage;
    exit 1);
  List.iter
    (fun path ->
      let parse () =
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
      if !bench_runs > 0 then begin
        for _ = 1 to !bench_runs do
          let r = parse () in
          if !resolve then begin
            let resolver = Vcd.Resolver.make r.header in
            let filter = Vcd_util.build_filter resolver patterns regexes in
            Seq.iter (function Change (id, _) when show_id filter id -> () | _ -> ()) r.simulation
          end
          else Seq.iter ignore r.simulation
        done
      end
      else begin
        let result = parse () in
        let h = result.header in
        if !resolve then begin
          let resolver = Vcd.Resolver.make h in
          let filter = Vcd_util.build_filter resolver patterns regexes in
          let strip_map = if !strip then Vcd_util.build_strip_map resolver filter else None in
          let resolve id =
            match Vcd.Resolver.find resolver id with
            | None -> ([ Vcd_types.ID.to_string id ], 0)
            | Some entry ->
                let size = Vcd.Resolver.entry_size entry in
                let names =
                  Vcd.Resolver.Ref_set.fold
                    (fun r acc ->
                      let display = match strip_map with None -> r | Some f -> f r in
                      Vcd_types.Reference.to_string display :: acc)
                    (Vcd.Resolver.entry_references entry) []
                in
                (names, size)
          in
          let cur_in_range = ref (ranges = []) in
          let pending_ts = ref None in
          let flush () =
            match !pending_ts with
            | None -> ()
            | Some t ->
                Format.printf "#%a\n" Vcd_types.Timestamp.pp t;
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
                    List.iter (fun name -> Format.printf "  %s=%s\n" name (Vcd_types.Value.to_string_hex size v)) names
                | Change _ -> ()
                | DumpStart k when !cur_in_range ->
                    flush ();
                    Format.printf "$%s\n" k
                | DumpStart _ -> ()
                | DumpEnd when !cur_in_range -> Format.printf "$end\n"
                | DumpEnd -> ()
                | SimComment c when !cur_in_range ->
                    flush ();
                    Format.printf "//%s\n" c
                | SimComment _ -> ())
              result.simulation
          with Exit -> ()
        end
        else begin
          Printf.printf "file:      %s\n" (Filename.basename path);
          Printf.printf "date:      %s\n" (opt Fun.id h.date);
          Printf.printf "version:   %s\n" (opt Fun.id h.version);
          Printf.printf "timescale: %s\n" (opt Fun.id h.timescale);
          if !summary then begin
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
        end
      end)
    files
