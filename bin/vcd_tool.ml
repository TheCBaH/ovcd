module Stateful = Vcd.Stateful
module ID_set = Vcd_util.ID_set

let pp_changes resolver strip_map id_set ev =
  Stateful.State.iter
    (fun id v ->
      match Vcd.Resolver.find resolver id with
      | None -> ()
      | Some entry ->
          let show = match id_set with None -> true | Some s -> ID_set.mem id s in
          if show then begin
            let size = Vcd.Resolver.entry_size entry in
            Vcd.Resolver.Ref_set.iter
              (fun r ->
                let display = match strip_map with None -> r | Some f -> f r in
                Format.printf "  %s=%s\n" (Vcd_types.Reference.to_string display) (Vcd_types.Value.to_string_hex size v))
              (Vcd.Resolver.entry_references entry)
          end)
    ev.Stateful.changes

let () =
  let ranges = ref [] in
  let patterns = ref [] in
  let regexes = ref [] in
  let strip = ref false in
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
      ("--strip", Arg.Set strip, " Strip the longest common scope prefix from signal names");
    ]
  in
  let usage =
    "usage: vcd_tool [--range SPEC]... [--signal PATTERN]... [--signal-re REGEX]... [--strip] <file.vcd>..."
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
      let filter = Vcd_util.build_filter resolver patterns regexes in
      let strip_map = if !strip then Vcd_util.build_strip_map resolver filter else None in
      let events =
        match filter with
        | None -> Stateful.stream ~ranges result.simulation
        | Some ids -> Stateful.stream ~reported:ids ~ranges result.simulation
      in
      Seq.iter
        (fun ev ->
          Format.printf "#%a\n" Vcd_types.Timestamp.pp ev.Stateful.time;
          pp_changes resolver strip_map filter ev)
        events)
    files
