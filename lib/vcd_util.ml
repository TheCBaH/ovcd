[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-sonnet-4-6"]
[@@@ai_provider "Anthropic"]

module ID_set = Vcd.ID_set
module ID_map = Vcd.ID_map
module Ref_map = Vcd.Ref_map

(* Map from each matched ID to the specific subset of its references that
   the user's patterns selected. Kept separate from ID_set so display code
   can avoid printing unselected aliases of a matched ID. *)
type filter = Vcd.Ref_set.t ID_map.t
type unmatched = { patterns : string list; regexes : string list }

let add_refs id refs m =
  ID_map.update id (function None -> Some refs | Some old -> Some (Vcd.Ref_set.union old refs)) m

let build_filter resolver patterns regexes =
  if patterns = [] && regexes = [] then (None, { patterns = []; regexes = [] })
  else begin
    let id_map, no_pattern =
      List.fold_left
        (fun (m, missed) (s, pat) ->
          match Vcd.Resolver.find_all pat resolver with
          | [] -> (m, s :: missed)
          | entries ->
              ( List.fold_left
                  (fun m e -> add_refs (Vcd.Resolver.entry_id e) (Vcd.Resolver.entry_references e) m)
                  m entries,
                missed ))
        (ID_map.empty, []) patterns
    in
    let res = Array.of_list regexes in
    let hit = Array.make (Array.length res) false in
    let ref_matches r =
      let name = Vcd_types.Reference.to_string r in
      let any = ref false in
      Array.iteri
        (fun i (_, re) ->
          if Re.execp re name then begin
            hit.(i) <- true;
            any := true
          end)
        res;
      !any
    in
    let id_map =
      if regexes = [] then id_map
      else
        Vcd.Resolver.fold
          (fun entry acc ->
            let refs = Vcd.Ref_set.filter ref_matches (Vcd.Resolver.entry_references entry) in
            if Vcd.Ref_set.is_empty refs then acc else add_refs (Vcd.Resolver.entry_id entry) refs acc)
          resolver id_map
    in
    let no_regex = List.filteri (fun i _ -> not hit.(i)) regexes |> List.map fst in
    (Some id_map, { patterns = List.rev no_pattern; regexes = no_regex })
  end

let filter_ids f = ID_map.fold (fun id _ acc -> ID_set.add id acc) f ID_set.empty
let mem_filter filter id = match filter with None -> true | Some m -> ID_map.mem id m

let selected_refs resolver filter id =
  match filter with
  | None -> Vcd.Resolver.references resolver id
  | Some m -> Option.value ~default:Vcd.Ref_set.empty (ID_map.find_opt id m)

let build_strip_map resolver filter =
  let matched =
    match filter with
    | None ->
        Vcd.Resolver.fold
          (fun entry acc -> Vcd.Ref_set.fold (fun r a -> r :: a) (Vcd.Resolver.entry_references entry) acc)
          resolver []
    | Some m -> ID_map.fold (fun _ refs acc -> Vcd.Ref_set.fold (fun r a -> r :: a) refs acc) m []
  in
  let rec common2 = function x :: xs, y :: ys when String.equal x y -> x :: common2 (xs, ys) | _ -> [] in
  let lists = List.rev_map Vcd_types.Reference.to_list matched in
  let prefix = match lists with [] -> [] | first :: rest -> List.fold_left (fun p l -> common2 (p, l)) first rest in
  let min_len = List.fold_left (fun m l -> min m (List.length l)) max_int lists in
  let prefix = List.filteri (fun i _ -> i < min (List.length prefix) (max 0 (min_len - 1))) prefix in
  match prefix with
  | [] -> None
  | _ ->
      let rec drop_prefix = function
        | [], rest | _ :: _, ([] as rest) -> rest
        | _ :: pfx, _ :: comps -> drop_prefix (pfx, comps)
      in
      let tbl =
        List.fold_left
          (fun m r ->
            let stripped = Vcd_types.Reference.of_list (drop_prefix (prefix, Vcd_types.Reference.to_list r)) in
            Ref_map.add r stripped m)
          Ref_map.empty matched
      in
      Some (fun r -> Option.value ~default:r (Ref_map.find_opt r tbl))

let parse_range s : Vcd.time_range =
  let ts_of str =
    let str = String.trim str in
    if str = "" then None
    else
      match Int64.of_string_opt str with
      | Some t when String.for_all (fun c -> c >= '0' && c <= '9') str -> Some (Vcd_types.Timestamp.of_int64 t)
      | _ -> failwith (Printf.sprintf "invalid time %S" str)
  in
  let checked (r : Vcd.time_range) =
    match (r.start, r.stop) with
    | Some a, Some b when Vcd_types.Timestamp.compare a b > 0 ->
        failwith (Format.asprintf "start %a is after stop %a" Vcd_types.Timestamp.pp a Vcd_types.Timestamp.pp b)
    | _ -> r
  in
  let starts_with2 s = String.length s >= 2 && s.[0] = '.' && s.[1] = '.' in
  let ends_with2 s =
    let n = String.length s in
    n >= 2 && s.[n - 2] = '.' && s.[n - 1] = '.'
  in
  let strip_leading_dots s =
    let i = ref 0 in
    while !i < String.length s && s.[!i] = '.' do
      incr i
    done;
    String.sub s !i (String.length s - !i)
  in
  let strip_trailing_dots s =
    let n = String.length s in
    let i = ref n in
    while !i > 0 && s.[!i - 1] = '.' do
      decr i
    done;
    String.sub s 0 !i
  in
  let s = String.trim s in
  if s = "" then failwith "empty time range";
  checked
  @@
  if starts_with2 s then
    let rest = String.trim (strip_leading_dots s) in
    let rest =
      if String.length rest > 0 && rest.[0] = '-' then String.trim (String.sub rest 1 (String.length rest - 1))
      else rest
    in
    Vcd.{ start = None; stop = ts_of rest }
  else if ends_with2 s then
    let rest = String.trim (strip_trailing_dots s) in
    let rest =
      let n = String.length rest in
      if n > 0 && rest.[n - 1] = '-' then String.trim (String.sub rest 0 (n - 1)) else rest
    in
    Vcd.{ start = ts_of rest; stop = None }
  else
    match String.index_opt s ':' with
    | Some i -> Vcd.{ start = ts_of (String.sub s 0 i); stop = ts_of (String.sub s (i + 1) (String.length s - i - 1)) }
    | None -> (
        match String.index_opt s '-' with
        | None ->
            let t = Option.get (ts_of s) in
            Vcd.{ start = Some t; stop = Some t }
        | Some i ->
            Vcd.{ start = ts_of (String.sub s 0 i); stop = ts_of (String.sub s (i + 1) (String.length s - i - 1)) })

let in_ranges = Vcd.in_ranges
let past_all_ranges = Vcd.past_all_ranges
let error_message : Vcd.Error.error -> string = function `Io m -> m | e -> "parse error: " ^ Vcd.Error.to_string e
