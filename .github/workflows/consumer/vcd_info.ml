module Stateful = Vcd.Stateful

let pp_changes resolver filter ev =
  Vcd.ID_map.iter
    (fun id v ->
      match Vcd.Resolver.find resolver id with
      | None -> ()
      | Some entry ->
          let size = Vcd.Resolver.entry_size entry in
          Vcd.Ref_set.iter
            (fun r ->
              Format.printf "  %s=%s\n" (Vcd_types.Reference.to_string r) (Vcd_types.Value.to_string_hex size v))
            (Vcd_util.selected_refs resolver filter id))
    ev.Stateful.changes

let () =
  let patterns = ref [] in
  let regexes = ref [] in
  let ranges = ref [] in
  let files = ref [] in
  let spec =
    [
      ( "--signal",
        Arg.String
          (fun s ->
            match Filter_parser.parse s with
            | Error msg ->
                Printf.eprintf "error: invalid signal pattern %S: %s\n" s msg;
                exit 1
            | Ok pat -> patterns := (s, pat) :: !patterns),
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
      ( "--range",
        Arg.String
          (fun s ->
            match Vcd_util.parse_range s with
            | exception Failure msg ->
                Printf.eprintf "error: invalid --range %S: %s\n" s msg;
                exit 1
            | r -> ranges := r :: !ranges),
        "SPEC  Restrict to a time range, e.g. \"0-500\" or \"...1000\" (may be repeated)" );
    ]
  in
  let usage = "usage: vcd_info [--signal PATTERN]... [--signal-re REGEX]... [--range SPEC]... <file.vcd>..." in
  Arg.parse spec (fun f -> files := f :: !files) usage;
  let files = List.rev !files in
  let patterns = List.rev !patterns in
  let regexes = List.rev !regexes in
  let ranges = List.rev !ranges in
  if files = [] then (
    Arg.usage spec usage;
    exit 1);
  List.iter
    (fun path ->
      let r =
        match Vcd.parse_file path with
        | r -> r
        | exception Vcd.Parse_error msg ->
            Printf.eprintf "parse error: %s\n" msg;
            exit 1
        | exception Sys_error msg ->
            Printf.eprintf "%s\n" msg;
            exit 1
      in
      let resolver = Vcd.Resolver.make r.header in
      let filter = Vcd_util.build_filter resolver patterns regexes in
      let count = Vcd.Resolver.fold (fun _ n -> n + 1) resolver 0 in
      Printf.printf "Signals: %d\n" count;
      let events =
        match filter with
        | None -> Stateful.stream ~ranges r.simulation
        | Some f -> Stateful.stream ~reported:(Vcd_util.filter_ids f) ~ranges r.simulation
      in
      Seq.iter
        (fun ev ->
          Format.printf "#%a\n" Vcd_types.Timestamp.pp ev.Stateful.time;
          pp_changes resolver filter ev)
        events)
    files
