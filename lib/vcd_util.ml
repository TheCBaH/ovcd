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

let ref_matches_any patterns regexes r =
  List.exists (fun (_, pat) -> Filter_matcher.matches pat r) patterns
  ||
  match regexes with
  | [] -> false
  | _ -> List.exists (fun (_, re) -> Re.execp re (Vcd_types.Reference.to_string r)) regexes

let build_filter resolver patterns regexes =
  if patterns = [] && regexes = [] then None
  else begin
    let id_map =
      Vcd.Resolver.fold
        (fun entry acc ->
          let matched_refs =
            Vcd.Ref_set.filter (ref_matches_any patterns regexes) (Vcd.Resolver.entry_references entry)
          in
          if Vcd.Ref_set.is_empty matched_refs then acc else ID_map.add (Vcd.Resolver.entry_id entry) matched_refs acc)
        resolver ID_map.empty
    in
    let any_ref_matches f =
      Vcd.Resolver.fold (fun e b -> b || Vcd.Ref_set.exists f (Vcd.Resolver.entry_references e)) resolver false
    in
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
    Some id_map
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
    if str = "" then None else Some (Vcd_types.Timestamp.of_string str)
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
    match String.index_opt s '-' with
    | None ->
        let t = Option.get (ts_of s) in
        Vcd.{ start = Some t; stop = Some t }
    | Some i -> Vcd.{ start = ts_of (String.sub s 0 i); stop = ts_of (String.sub s (i + 1) (String.length s - i - 1)) }

let in_ranges = Vcd.in_ranges
let past_all_ranges = Vcd.past_all_ranges
