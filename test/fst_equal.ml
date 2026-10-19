[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5-5"]
[@@@ai_provider "Anthropic"]

(* fst_equal FST VCD [N]: the FST must parse to the header and events of VCD, its fst2vcd -e conversion. Reals are
   compared at the 16 digits fst2vcd prints. With N, both are parsed selecting the IDs whose hash is 0 mod N, and must ask
   [select] the same, once per ID, only from the first pull. *)

let rounded = function
  | Vcd_ast.Change (id, Vcd_types.Value.Real d) ->
      Vcd_ast.Change (id, Vcd_types.Value.Real (float_of_string (Printf.sprintf "%.16g" d)))
  | e -> e

let same a b =
  match (a, b) with
  | Vcd_ast.Change (i, Vcd_types.Value.Real x), Vcd_ast.Change (j, Vcd_types.Value.Real y) ->
      Vcd_types.ID.equal i j && (x = y || (Float.is_nan x && Float.is_nan y))
  | _ -> a = b

let show = function
  | Vcd_ast.Timestamp t -> Format.asprintf "#%a" Vcd_types.Timestamp.pp t
  | Change (id, v) -> Format.asprintf "%s=%s" (Vcd_types.ID.to_string id) (Vcd_types.Value.to_string v)
  | DumpStart s -> "$" ^ s
  | DumpEnd -> "$end"
  | SimComment c -> "$comment " ^ c

let () =
  let fst = Sys.argv.(1) and vcd = Sys.argv.(2) in
  if Vcd_fst.detect_file fst <> `Fst then print_endline "FST not detected";
  if Vcd_fst.detect_file vcd <> `Vcd then print_endline "VCD not detected";
  let every = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 0 in
  let selector () =
    let asked = Hashtbl.create 64 and n = ref 0 in
    let select id =
      let k = Vcd_types.ID.to_string id in
      Hashtbl.replace asked k (1 + Option.value (Hashtbl.find_opt asked k) ~default:0);
      incr n;
      Hashtbl.hash k mod every = 0
    in
    (select, asked, n)
  in
  let sa, asked_a, na = selector () and sb, asked_b, nb = selector () in
  let select f = if every > 0 then Some f else None in
  let a = Vcd_fst.parse_file ?select:(select sa) fst and b = Vcd.parse_file ?select:(select sb) vcd in
  if a.header <> b.header then print_endline "headers differ";
  if !na + !nb > 0 then print_endline "select called before the first pull";
  let a = { a with simulation = Seq.memoize a.simulation } and b = { b with simulation = Seq.memoize b.simulation } in
  ignore (a.simulation (), b.simulation ());
  if every > 0 then begin
    let keys h = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) h []) in
    if keys asked_a <> keys asked_b then print_endline "FST and VCD ask select about different IDs";
    if Hashtbl.fold (fun _ c ok -> ok && c = 1) asked_a true && Hashtbl.fold (fun _ c ok -> ok && c = 1) asked_b true
    then
      Printf.printf "%d IDs asked once, %d selected\n" (Hashtbl.length asked_a)
        (List.length (List.filter (fun k -> Hashtbl.hash k mod every = 0) (keys asked_a)))
    else print_endline "an ID asked twice"
  end;
  let rec go n x y =
    match (x (), y ()) with
    | Seq.Nil, Seq.Nil -> Printf.printf "%d events\n" n
    | Seq.Cons (e, x), Seq.Cons (f, y) ->
        if same (rounded e) f then go (n + 1) x y else Printf.printf "event %d differs: %s vs %s\n" n (show e) (show f)
    | Seq.Cons (e, _), Seq.Nil -> Printf.printf "event %d only in the FST: %s\n" n (show e)
    | Seq.Nil, Seq.Cons (f, _) -> Printf.printf "event %d only in the VCD: %s\n" n (show f)
  in
  go 0 a.simulation b.simulation
