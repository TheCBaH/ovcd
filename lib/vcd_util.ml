module ID_set = Set.Make (Vcd_types.ID)
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
