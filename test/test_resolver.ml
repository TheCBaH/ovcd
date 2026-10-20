open Vcd_types

(* The resolver as it was before its path index became a hash table, kept to compare against. *)
module Old = struct
  open Vcd

  module Ref_fwd_map = Map.Make (struct
    type t = string list

    let compare = List.compare String.compare
  end)

  type entry = { id : ID.t; size : int; references : Ref_set.t }
  type t = { by_id : entry ID_map.t; by_ref : ID.t Ref_map.t; by_ref_fwd : ID.t Ref_fwd_map.t }

  let make (header : Vcd_ast.header) =
    let open Reference in
    let rec collect path (vars : Vcd_ast.var_decl list) (children : Vcd_ast.scope_node list) (by_id, by_ref, by_ref_fwd)
        =
      let acc =
        List.fold_left
          (fun (by_id, by_ref, by_ref_fwd) (var : Vcd_ast.var_decl) ->
            let r = push var.ref path in
            let entry =
              match ID_map.find_opt var.id by_id with
              | None -> { id = var.id; size = var.size; references = Ref_set.singleton r }
              | Some e -> { e with references = Ref_set.add r e.references }
            in
            (ID_map.add var.id entry by_id, Ref_map.add r var.id by_ref, Ref_fwd_map.add (to_list r) var.id by_ref_fwd))
          (by_id, by_ref, by_ref_fwd) vars
      in
      List.fold_left
        (fun acc (c : Vcd_ast.scope_node) -> collect (push c.s_name path) c.vars c.children acc)
        acc children
    in
    let by_id, by_ref, by_ref_fwd =
      List.fold_left
        (fun acc (s : Vcd_ast.scope_node) -> collect (push s.s_name empty) s.vars s.children acc)
        (ID_map.empty, Ref_map.empty, Ref_fwd_map.empty)
        header.scopes
    in
    { by_id; by_ref; by_ref_fwd }

  let fold f t acc = ID_map.fold (fun _ e a -> f e a) t.by_id acc

  let list_starts_with prefix lst =
    let rec go = function
      | [], _ -> true
      | _ :: _, [] -> false
      | ph :: pt, lh :: lt -> String.equal ph lh && go (pt, lt)
    in
    go (prefix, lst)

  let collect_by_tail tail_fwd t =
    let lb = Reference.of_list tail_fwd in
    let ub = Reference.of_list ((List.hd tail_fwd ^ "\x00") :: List.tl tail_fwd) in
    let rec go acc s =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons ((k, id), rest) -> if Reference.compare k ub < 0 then go (ID_set.add id acc) rest else acc
    in
    go ID_set.empty (Ref_map.to_seq_from lb t.by_ref)

  let collect_by_prefix prefix_fwd t =
    let rec go acc s =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons ((k, id), rest) -> if list_starts_with prefix_fwd k then go (ID_set.add id acc) rest else acc
    in
    go ID_set.empty (Ref_fwd_map.to_seq_from prefix_fwd t.by_ref_fwd)

  let find_all pat t =
    let head, tail = Filter_matcher.anchors pat in
    let add_if_match e acc =
      let refs = Ref_set.filter (Filter_matcher.matches pat) e.references in
      if Ref_set.is_empty refs then acc else { e with references = refs } :: acc
    in
    match (head, tail) with
    | [], [] -> ID_map.fold (fun _ e acc -> add_if_match e acc) t.by_id []
    | _ ->
        let candidate_ids = match tail with [] -> collect_by_prefix head t | _ -> collect_by_tail tail t in
        ID_set.fold
          (fun id acc -> match ID_map.find_opt id t.by_id with None -> acc | Some e -> add_if_match e acc)
          candidate_ids []
end

let describe_new e =
  ( ID.to_string (Vcd.Resolver.entry_id e),
    Vcd.Resolver.entry_size e,
    Vcd.Ref_set.elements (Vcd.Resolver.entry_references e) )

let describe_old (e : Old.entry) = (ID.to_string e.id, e.size, Vcd.Ref_set.elements e.references)

let patterns =
  [
    "**"; "top.**"; "**.clk"; "*.clk"; "top.*.clk"; "top.a.b.*"; "top.s1.**"; "**.x"; "top.nothing.**"; "a.b.**"; "**.c";
  ]

(* Compares every query of the two resolvers; returns the first difference. *)
let compare_resolvers (header : Vcd_ast.header) =
  let n = Vcd.Resolver.make header and o = Old.make header in
  let fail what = Some what in
  let folded_new = List.rev (Vcd.Resolver.fold (fun e acc -> describe_new e :: acc) n [])
  and folded_old = List.rev (Old.fold (fun e acc -> describe_old e :: acc) o []) in
  if folded_new <> folded_old then fail "fold"
  else
    let ids = List.map (fun (id, _, _) -> ID.of_string id) folded_old @ [ ID.of_string "~missing" ] in
    let bad_id =
      List.find_opt
        (fun id ->
          Option.map describe_new (Vcd.Resolver.find n id) <> Option.map describe_old (Vcd.ID_map.find_opt id o.by_id)
          || Vcd.Ref_set.elements (Vcd.Resolver.references n id)
             <> Vcd.Ref_set.elements
                  (match Vcd.ID_map.find_opt id o.by_id with None -> Vcd.Ref_set.empty | Some e -> e.references))
        ids
    in
    if bad_id <> None then fail "find/references"
    else
      let refs = Vcd.Ref_map.bindings o.by_ref |> List.map fst in
      let missing = [ Reference.of_string "no.such.path"; Reference.of_list [ "top"; "a.b" ] ] in
      let bad_ref =
        List.find_opt
          (fun r -> Vcd.Resolver.find_id n r <> Option.map Fun.id (Vcd.Ref_map.find_opt r o.by_ref))
          (refs @ missing)
      in
      if bad_ref <> None then fail "find_id"
      else
        let bad_pat =
          List.find_opt
            (fun p ->
              let pat = Filter_parser.parse_exn p in
              let sort = List.sort compare in
              sort (List.map describe_new (Vcd.Resolver.find_all pat n))
              <> sort (List.map describe_old (Old.find_all pat o)))
            patterns
        in
        Option.map (fun p -> "find_all " ^ p) bad_pat

let var ?(size = 1) ?(ty = Vcd_ast.Wire) id ref : Vcd_ast.var_decl =
  { v_type = ty; size; id = ID.of_string id; ref; index = None }

let scope name vars children : Vcd_ast.scope_node = { s_type = Vcd_ast.Module; s_name = name; children; vars }

let header scopes : Vcd_ast.header =
  { version = None; date = None; timescale = None; timezero = None; comment = None; scopes }

let%test "declaration order differs from ID order; alias sizes differ" =
  (* "z" is declared first but sorts after "a"; "a" is an alias whose first declaration decides the size. *)
  let h =
    header
      [
        scope "top"
          [ var "z" "clk"; var ~size:8 "a" "data" ]
          [ scope "s1" [ var ~size:4 "a" "data_alias"; var "m" "clk" ] []; scope "s0" [ var "z" "clk" ] [] ];
      ]
  in
  let n = Vcd.Resolver.make h in
  let ids = Vcd.Resolver.fold (fun e acc -> ID.to_string (Vcd.Resolver.entry_id e) :: acc) n [] |> List.rev in
  ids = [ "a"; "m"; "z" ]
  && (match Vcd.Resolver.find n (ID.of_string "a") with Some e -> Vcd.Resolver.entry_size e = 8 | None -> false)
  && compare_resolvers h = None

let%test "a scope name with a dot is not the nested path" =
  let h = header [ scope "top" [] [ scope "a.b" [ var "p" "x" ] []; scope "a" [] [ scope "b" [ var "q" "x" ] [] ] ] ] in
  let n = Vcd.Resolver.make h in
  Vcd.Resolver.find_id n (Reference.of_list [ "top"; "a.b"; "x" ]) = Some (ID.of_string "p")
  && Vcd.Resolver.find_id n (Reference.of_list [ "top"; "a"; "b"; "x" ]) = Some (ID.of_string "q")
  && compare_resolvers h = None

let%test "the same path declared twice keeps the last ID" =
  let h = header [ scope "top" [ var "p" "x"; var "q" "x" ] [] ] in
  Vcd.Resolver.find_id (Vcd.Resolver.make h) (Reference.of_list [ "top"; "x" ]) = Some (ID.of_string "q")
  && compare_resolvers h = None

let%test "a header without variables" =
  compare_resolvers (header []) = None && compare_resolvers (header [ scope "top" [] [] ]) = None

(* Seeded random headers: aliases across scopes, repeated and dotted names, deep paths. *)
let random_header st =
  let names = [| "top"; "a"; "b"; "c"; "s1"; "a.b"; "clk"; "x" |] in
  let name () = names.(Random.State.int st (Array.length names)) in
  let id () = String.make 1 (Char.chr (Char.code 'A' + Random.State.int st 40)) in
  let rec sc depth =
    let vars = List.init (Random.State.int st 5) (fun _ -> var ~size:(1 + Random.State.int st 16) (id ()) (name ())) in
    let children = if depth = 0 then [] else List.init (Random.State.int st 3) (fun _ -> sc (depth - 1)) in
    scope (name ()) vars children
  in
  header (List.init (1 + Random.State.int st 2) (fun _ -> sc (1 + Random.State.int st 5)))

let%test "random headers equal the old resolver" =
  let st = Random.State.make [| 20261020 |] in
  let rec go i = i = 300 || (compare_resolvers (random_header st) = None && go (i + 1)) in
  go 0
