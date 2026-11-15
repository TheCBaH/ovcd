let fail fmt =
  Printf.ksprintf
    (fun s ->
      Tool_out.flush ();
      prerr_endline s;
      exit 1)
    fmt

let reject cond msg = if cond then fail "error: %s" msg
let display strip_map r = match strip_map with None -> r | Some f -> f r

let add_pattern r s =
  match Filter_parser.parse s with
  | Error msg -> fail "error: invalid signal pattern %S: %s" s msg
  | Ok pat -> r := (s, pat) :: !r

type selection = {
  ranges : Vcd.time_range list ref;
  patterns : (string * Filter_ast.pattern) list ref;
  regexes : (string * Re.re) list ref;
}

let selection () = { ranges = ref []; patterns = ref []; regexes = ref [] }

let selection_specs ~signal_doc ~regex_doc sel =
  [
    ( "--range",
      Arg.String
        (fun s ->
          match Vcd_util.parse_range s with
          | exception Failure msg -> fail "error: invalid --range %S: %s" s msg
          | r -> sel.ranges := r :: !(sel.ranges)),
      "SPEC  Restrict to a time range, e.g. \"...1000\" or \"0-500\" or \"2000-...\"; may be repeated" );
    ("--signal", Arg.String (add_pattern sel.patterns), signal_doc);
    ( "--signal-re",
      Arg.String
        (fun s ->
          match Re.Pcre.re s |> Re.compile with
          | exception exn -> fail "error: invalid regex %S: %s" s (Printexc.to_string exn)
          | re -> sel.regexes := (s, re) :: !(sel.regexes)),
      regex_doc );
  ]

let parse_args spec usage =
  let files = ref [] in
  Arg.parse spec (fun f -> files := f :: !files) usage;
  if !files = [] then (
    Arg.usage spec usage;
    exit 1);
  List.rev !files

let warn_unmatched ~label (u : Vcd_util.unmatched) =
  List.iter (Printf.eprintf "warning: %s %S matched no signals\n" label) u.patterns;
  List.iter (Printf.eprintf "warning: --signal-re %S matched no signals\n") u.regexes

let with_parsed ?select path f =
  match Vcd_fst.with_file ?select path f with
  | v -> v
  | exception Vcd.Error.E e -> fail "%s" (Vcd_util.error_message e)
  | exception Failure msg -> fail "error: %s" msg

let run main = try main () with Vcd.Error.E e -> fail "%s" (Vcd_util.error_message e)
