open Filter_ast

(* Glob match within a single component.
   parts is the Name_pattern payload: the segment string split on '*',
   e.g. ["ba"; ""] for ba* or [""; "ar"] for *ar.
   Each '*' between consecutive parts matches any substring. *)
let glob_match parts str =
  let ns = String.length str in
  let rec glob_match_aux parts pos =
    match parts with
    | [] -> pos = ns
    | [ last ] ->
        let llen = String.length last in
        llen <= ns - pos && String.sub str (ns - llen) llen = last
    | part :: rest ->
        let plen = String.length part in
        let rec find_from i =
          i + plen <= ns && ((String.sub str i plen = part && glob_match_aux rest (i + plen)) || find_from (i + 1))
        in
        find_from pos
  in
  glob_match_aux parts 0

let anchors pat =
  let rec take_lits = function [] -> [] | Literal s :: rest -> s :: take_lits rest | _ -> [] in
  let head = take_lits pat in
  let tail = List.rev (take_lits (List.rev pat)) in
  (head, tail)

let matches pattern ref =
  let rec matches_aux pats cs =
    match (pats, cs) with
    | [], [] -> true
    | [], _ -> false
    | Glob :: rest_p, _ -> (
        (* Glob matches 0 components, OR grows by consuming one more *)
        matches_aux rest_p cs
        || match cs with [] -> false | _ :: rest_c -> matches_aux (Glob :: rest_p) rest_c)
    | _, [] -> false
    | Wildcard :: rest_p, _ :: rest_c -> matches_aux rest_p rest_c
    | Literal s :: rest_p, c :: rest_c -> String.equal s c && matches_aux rest_p rest_c
    | Alt ss :: rest_p, c :: rest_c -> List.mem c ss && matches_aux rest_p rest_c
    | Name_pattern parts :: rest_p, c :: rest_c -> glob_match parts c && matches_aux rest_p rest_c
  in
  matches_aux pattern (Vcd_types.Reference.to_list ref)
