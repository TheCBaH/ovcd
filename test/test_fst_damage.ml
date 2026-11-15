(* Damaged FST files through every Vcd_fst entry point: only Vcd.Error.E may escape, from opening or from pulling the
   whole simulation. The malformed files are libfst's (tests/data/malformed). *)

let read path = In_channel.with_open_bin path In_channel.input_all
let kitchen = read "data/fst2vcd/kitchen.fst"
let malformed_dir = "data/malformed"
let escapes = ref []
let accepted = ref 0
let rejected = ref 0

let guard input f =
  try
    f ();
    incr accepted
  with
  | Vcd.Error.E e ->
      incr rejected;
      ignore (Vcd.Error.to_string e)
  | e -> escapes := (Printexc.to_string e, String.length input) :: !escapes

let drain (r : Vcd.parse_result) = Seq.iter ignore r.simulation

let with_tmp s f =
  let path = Filename.temp_file "damaged" ".fst" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun oc -> output_string oc s);
      f path)

let exercise_string s =
  guard s (fun () -> drain (Vcd_fst.parse_string s));
  guard s (fun () -> drain (Vcd_fst.open_string s));
  guard s (fun () -> drain (Vcd_fst.parse_string ~select:(fun _ -> true) s));
  guard s (fun () -> ignore (Vcd_fst.detect_string s))

let exercise_file s =
  with_tmp s (fun path ->
      guard s (fun () -> drain (Vcd_fst.parse_file path));
      guard s (fun () -> drain (Vcd_fst.open_file path));
      guard s (fun () -> Vcd_fst.with_file path drain);
      guard s (fun () -> ignore (Vcd_fst.blocks path));
      List.iter
        (fun t ->
          guard s (fun () ->
              let state, r = Vcd_fst.open_at path ~time:(Vcd_types.Timestamp.of_int64 t) in
              Seq.iter ignore (Vcd.Stateful.stream ~init:state r.simulation)))
        [ 0L; 2000L; 1_000_000L ];
      guard s (fun () ->
          match Vcd_fst.File.open_ path with
          | Error e -> raise (Vcd.Error.E e)
          | Ok f ->
              Fun.protect
                ~finally:(fun () -> Vcd_fst.File.close f)
                (fun () ->
                  ignore (Vcd_fst.File.header f);
                  (match Vcd_fst.File.blocks f with
                  | Ok bs ->
                      Array.iter
                        (fun (t0, _) ->
                          let _, sim = Vcd_fst.File.stream ~at:t0 f in
                          Seq.iter ignore sim)
                        bs
                  | Error e -> raise (Vcd.Error.E e));
                  let _, sim = Vcd_fst.File.stream f in
                  Seq.iter ignore sim)));
  exercise_string s

let report () =
  Printf.printf "both outcomes seen: %b\n" (!accepted > 100 && !rejected > 100);
  Printf.printf "escapes: %d\n" (List.length !escapes);
  List.iteri (fun i (e, n) -> if i < 5 then Printf.printf "%s on %d bytes\n" e n) !escapes

let%expect_test "libfst's malformed files raise only Vcd.Error.E" =
  let names = Sys.readdir malformed_dir |> Array.to_list |> List.sort compare in
  List.iter
    (fun n ->
      let s = read (Filename.concat malformed_dir n) in
      exercise_file s;
      let outcome =
        match Vcd_fst.parse_string_result s with
        | Ok r -> (
            try
              drain r;
              "ok"
            with Vcd.Error.E _ -> "error while pulling")
        | Error _ -> "error"
      in
      Printf.printf "%s: %s\n" n outcome)
    names;
  Printf.printf "escapes: %d\n" (List.length !escapes);
  [%expect
    {|
    chain_shorter_than_its_length.fst: error
    chain_table_overrun.fst: error
    chain_table_pair_unterminated.fst: error
    chain_table_unterminated.fst: ok
    chain_uncompresses_past_traversal.fst: error
    rvat_chain_table_overrun.fst: error while pulling
    traversal_buffer_smaller_than_chain.fst: error
    traversal_buffer_smaller_than_chain_fastlz.fst: error
    uncompressed_chain_past_traversal.fst: error while pulling
    uncompressed_chain_shorter_than_its_varint.fst: error while pulling
    escapes: 0 |}]

let%expect_test "every prefix of a valid FST raises only Vcd.Error.E" =
  let n = String.length kitchen in
  let step = 1 in
  let i = ref 0 in
  while !i < n do
    exercise_string (String.sub kitchen 0 !i);
    if !i mod 20 = 0 then exercise_file (String.sub kitchen 0 !i);
    i := !i + step
  done;
  report ();
  [%expect {|
    both outcomes seen: true
    escapes: 0 |}]

let%expect_test "byte flips and cuts of a valid FST raise only Vcd.Error.E" =
  let st = Random.State.make [| 20261117 |] in
  for round = 1 to 1500 do
    let b = Bytes.of_string kitchen in
    for _ = 1 to 1 + Random.State.int st 6 do
      let p = Random.State.int st (Bytes.length b) in
      Bytes.set b p (Char.chr (Char.code (Bytes.get b p) lxor (1 lsl Random.State.int st 8)))
    done;
    let s = Bytes.to_string b in
    let s = if round mod 5 = 0 then String.sub s 0 (Random.State.int st (String.length s)) else s in
    if round mod 50 = 0 then exercise_file s else exercise_string s
  done;
  report ();
  [%expect {|
    both outcomes seen: true
    escapes: 0 |}]
