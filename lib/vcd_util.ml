module ID_set = Vcd.Stateful.ID_set
module Ref_map = Map.Make (Vcd_types.Reference)

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
    | Some i ->
        Vcd.{ start = ts_of (String.sub s 0 i); stop = ts_of (String.sub s (i + 1) (String.length s - i - 1)) }

let in_ranges ranges t =
  match ranges with
  | [] -> true
  | _ ->
      List.exists
        (fun (r : Vcd.time_range) ->
          (match r.start with None -> true | Some s -> Vcd_types.Timestamp.compare t s >= 0)
          && match r.stop with None -> true | Some e -> Vcd_types.Timestamp.compare t e <= 0)
        ranges

let past_all_ranges ranges t =
  match ranges with
  | [] -> false
  | _ ->
      List.for_all
        (fun (r : Vcd.time_range) -> match r.stop with None -> false | Some e -> Vcd_types.Timestamp.compare t e > 0)
        ranges
