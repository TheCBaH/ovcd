open Vcd_ast
module ID_set = Set.Make (Vcd_types.ID)
module Ref_map = Map.Make (Vcd_types.Reference)

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

(* Build an ID set from DSL patterns and/or compiled regexes.
   Warns for each pattern or regex that matched no signal. *)
let build_filter resolver patterns regexes =
  if patterns = [] && regexes = [] then None
  else begin
    let id_set =
      Vcd.Resolver.fold
        (fun entry acc ->
          let ref_ = Vcd.Resolver.entry_reference entry in
          if
            List.exists (fun (_, pat) -> Filter_matcher.matches pat ref_) patterns
            ||
            match regexes with
            | [] -> false
            | _ ->
                let name = Vcd_types.Reference.to_string ref_ in
                List.exists (fun (_, re) -> Re.execp re name) regexes
          then ID_set.add (Vcd.Resolver.entry_id entry) acc
          else acc)
        resolver ID_set.empty
    in
    let any_ref_matches f = Vcd.Resolver.fold (fun e b -> b || f (Vcd.Resolver.entry_reference e)) resolver false in
    List.iter
      (fun (s, pat) ->
        if not (any_ref_matches (Filter_matcher.matches pat)) then
          Printf.eprintf "warning: --signal %S matched no signals\n" s)
      patterns;
    List.iter
      (fun (s, re) ->
        if not (any_ref_matches (fun r -> Re.execp re (Vcd_types.Reference.to_string r))) then
          Printf.eprintf "warning: --signal-re %S matched no signals\n" s)
      regexes;
    Some id_set
  end

let show_id filter id = match filter with None -> true | Some s -> ID_set.mem id s

(* Longest common component prefix of a list of component-lists, guaranteed
   shorter than the shortest input (so no signal is stripped to empty). *)
let common_prefix_components lists =
  let rec common2 = function x :: xs, y :: ys when String.equal x y -> x :: common2 (xs, ys) | _ -> [] in
  match lists with
  | [] -> []
  | first :: rest ->
      let prefix = List.fold_left (fun p l -> common2 (p, l)) first rest in
      let min_len = List.fold_left (fun m l -> min m (List.length l)) max_int lists in
      List.filteri (fun i _ -> i < min (List.length prefix) (max 0 (min_len - 1))) prefix

(* Resolves all matched signals to References, finds the common prefix, and
   builds a Reference -> stripped_Reference map.  Returns None when the common
   prefix is empty.  filter=None means all signals matched. *)
let build_strip_map resolver filter =
  let matched =
    match filter with
    | None -> Vcd.Resolver.fold (fun entry acc -> Vcd.Resolver.entry_reference entry :: acc) resolver []
    | Some ids ->
        ID_set.fold
          (fun id acc ->
            match Vcd.Resolver.find resolver id with
            | Some entry -> Vcd.Resolver.entry_reference entry :: acc
            | None -> acc)
          ids []
  in
  match common_prefix_components (List.rev_map Vcd_types.Reference.to_list matched) with
  | [] -> None
  | prefix ->
      let rec drop_prefix_aux = function
        | [], rest | _ :: _, ([] as rest) -> rest
        | _ :: pfx, _ :: comps -> drop_prefix_aux (pfx, comps)
      in
      Some
        (List.fold_left
           (fun m r ->
             let stripped = Vcd_types.Reference.of_list (drop_prefix_aux (prefix, Vcd_types.Reference.to_list r)) in
             Ref_map.add r stripped m)
           Ref_map.empty matched)

let () =
  let summary = ref false in
  let bench_runs = ref 0 in
  let resolve = ref false in
  let strip = ref false in
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
    "usage: vcd_dump [--summary] [--bench N] [--resolve] [--strip] [--signal PATTERN]... [--signal-re REGEX]... \
     <file.vcd>"
  in
  Arg.parse spec (fun f -> files := f :: !files) usage;
  let files = List.rev !files in
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
            let filter = build_filter resolver patterns regexes in
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
          let filter = build_filter resolver patterns regexes in
          let strip_map = if !strip then build_strip_map resolver filter else None in
          (* Returns (display_name, signal_size) for an ID. *)
          let resolve id =
            match Vcd.Resolver.find resolver id with
            | None -> (Vcd_types.ID.to_string id, 0)
            | Some entry ->
                let r = Vcd.Resolver.entry_reference entry in
                let display =
                  match strip_map with None -> r | Some m -> Option.value ~default:r (Ref_map.find_opt r m)
                in
                (Vcd_types.Reference.to_string display, Vcd.Resolver.entry_size entry)
          in
          let pending_ts = ref None in
          let flush () =
            match !pending_ts with
            | None -> ()
            | Some t ->
                Format.printf "#%a\n" Vcd_types.Timestamp.pp t;
                pending_ts := None
          in
          Seq.iter
            (function
              | Timestamp t -> pending_ts := Some t
              | Change (id, v) when show_id filter id ->
                  flush ();
                  let name, size = resolve id in
                  Format.printf "  %s=%s\n" name (Vcd_types.Value.to_string_hex size v)
              | Change _ -> ()
              | DumpStart k ->
                  flush ();
                  Format.printf "$%s\n" k
              | DumpEnd -> Format.printf "$end\n"
              | SimComment c ->
                  flush ();
                  Format.printf "//%s\n" c)
            result.simulation
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
          let timestamps = ref 0 and changes = ref 0 and dumps = ref 0 and comments = ref 0 in
          Seq.iter
            (function
              | Timestamp _ -> incr timestamps
              | Change _ -> incr changes
              | DumpStart _ -> incr dumps
              | DumpEnd -> ()
              | SimComment _ -> incr comments)
            result.simulation;
          Printf.printf "events:    %d (timestamps: %d, changes: %d, dumps: %d, comments: %d)\n"
            (!timestamps + !changes + !dumps + !comments)
            !timestamps !changes !dumps !comments
        end
      end)
    files
