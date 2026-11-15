open Vcd_types

(* badframe.fst is kitchen.fst with the first byte of its second block's frame
   (the value of the first signal) replaced by 'Q'. *)
let%expect_test "a frame byte outside the 9-state alphabet is Other" =
  let state, _ = Vcd_fst.open_at "data/fst2vcd/badframe.fst" ~time:(Timestamp.of_int64 2000L) in
  (match Vcd.ID_map.min_binding_opt state with
  | Some (id, v) -> Printf.printf "%s=%s\n" (ID.to_string id) (Value.to_string v)
  | None -> print_endline "empty state");
  [%expect {| !=bQ |}]

let multi = "data/fst2vcd/kitchen.fst"
let open_file path = match Vcd_fst.File.open_ path with Ok f -> f | Error m -> failwith m
let pull_n n s = List.of_seq (Seq.take n s)
let rest s = match s () with Seq.Cons (_, k) -> k | Nil -> s

let%expect_test "repeated streams on one handle build the header once" =
  let f = open_file multi in
  let bs = Result.get_ok (Vcd_fst.File.blocks f) in
  Printf.printf "blocks>1 %b\n" (Array.length bs > 1);
  let h = Vcd_fst.File.header f in
  Array.iter
    (fun (t0, _) ->
      let _, s = Vcd_fst.File.stream ~at:t0 f in
      ignore (pull_n 3 s))
    bs;
  let _, s = Vcd_fst.File.stream f in
  ignore (pull_n 3 s);
  Printf.printf "builds %d same %b\n" (Vcd_fst.File.header_builds f) (h == Vcd_fst.File.header f);
  Vcd_fst.File.close f;
  [%expect {|
    blocks>1 true
    builds 1 same true |}]

let pull_error s =
  try
    ignore (Seq.length s);
    "ok"
  with Vcd.Parse_error m -> m

let%expect_test "a new stream or close invalidates earlier streams" =
  let f = open_file multi in
  let _, a = Vcd_fst.File.stream f in
  let a = rest a in
  let _, b = Vcd_fst.File.stream f in
  print_endline (pull_error a);
  let b = rest b in
  Vcd_fst.File.close f;
  print_endline (pull_error b);
  Vcd_fst.File.close f;
  (match Vcd_fst.File.stream f with _ -> () | exception Vcd.Parse_error m -> print_endline m);
  (match Vcd_fst.File.blocks f with Error m -> print_endline m | Ok _ -> print_endline "blocks ok");
  [%expect
    {|
    data/fst2vcd/kitchen.fst: stream superseded
    data/fst2vcd/kitchen.fst: handle closed
    data/fst2vcd/kitchen.fst: handle closed
    data/fst2vcd/kitchen.fst: handle closed |}]

let%expect_test "a stream borrows the handle" =
  let f = open_file multi in
  let _, a = Vcd_fst.File.stream f in
  let n = Seq.length a in
  let _, b = Vcd_fst.File.stream f in
  Printf.printf "%b %b\n" (n > 0) (Seq.length b = n);
  Vcd_fst.File.close f;
  [%expect {| true true |}]

let%expect_test "open_ of a missing file is an error" =
  (match Vcd_fst.File.open_ "data/fst2vcd/missing.fst" with
  | Error _ -> print_endline "error"
  | Ok _ -> print_endline "ok");
  [%expect {| error |}]
