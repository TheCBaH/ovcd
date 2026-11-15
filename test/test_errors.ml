let hdr = "$timescale 1ns $end\n$scope module m $end\n$var wire 1 ! a $end\n$upscope $end\n$enddefinitions $end\n"
let kitchen = "data/fst2vcd/kitchen.fst"

let show f =
  match Vcd.Error.catch f with Ok () -> print_endline "ok" | Error e -> Format.printf "%a\n" Vcd.Error.pp_error e

let pull s () = Seq.iter ignore (Vcd.parse_string s).Vcd.simulation

let with_temp contents k =
  let path = Filename.temp_file "errors" ".dat" in
  Out_channel.with_open_bin path (fun oc -> output_string oc contents);
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> k path)

(* Temp paths vary; print the error with the path replaced. *)
let show_path path f =
  match Vcd.Error.catch f with
  | Ok () -> print_endline "ok"
  | Error e ->
      let s = Vcd.Error.to_string e in
      let n = String.length path in
      let rec sub i =
        if i + n > String.length s then s
        else if String.sub s i n = path then String.sub s 0 i ^ "<path>" ^ String.sub s (i + n) (String.length s - i - n)
        else sub (i + 1)
      in
      print_endline (sub 0)

let%expect_test "header errors carry a position" =
  show (fun () -> ignore (Vcd.parse_string "$scope module $end $enddefinitions $end"));
  show (fun () -> ignore (Vcd.parse_string "\n\n$timezero soon $end\n$enddefinitions $end"));
  show (fun () -> ignore (Vcd.parse_string "$scope module m $end\n$var wire x ! a $end\n$enddefinitions $end"));
  show (fun () -> ignore (Vcd.parse_string "$scope module m $end\n$var wire 1 ! a $end\n$var wire y ! b $end"));
  [%expect
    {|
    syntax error at <string> line 1 col 18
    invalid $timezero: soon at <string> line 3 col 0
    invalid var size: x at <string> line 2 col 0
    invalid var size: y at <string> line 3 col 0 |}]

let%expect_test "value change errors carry a position, when pulled" =
  show (pull (hdr ^ "#0\n1!\n  %oops\n"));
  show (pull (hdr ^ "#0\n$comment never ended\n"));
  show (pull (hdr ^ "#0\n$comment x\n$end\n#99999999999999999999\n"));
  [%expect
    {|
    unexpected '%' at <string> line 8 col 2
    unexpected EOF inside keyword block at <string> line 8 col 0
    invalid timestamp 99999999999999999999 at <string> line 9 col 0 |}]

let%expect_test "the position names the file or the channel" =
  with_temp (hdr ^ "#0\n%\n") (fun path ->
      show_path path (fun () -> Seq.iter ignore (Vcd.parse_file path).Vcd.simulation);
      show_path path (fun () -> Vcd.with_file path (fun r -> Seq.iter ignore r.Vcd.simulation));
      In_channel.with_open_bin path (fun ic -> show (fun () -> Seq.iter ignore (Vcd.parse_channel ic).Vcd.simulation)));
  [%expect
    {|
    unexpected '%' at <path> line 7 col 0
    unexpected '%' at <path> line 7 col 0
    unexpected '%' at <channel> line 7 col 0 |}]

let%expect_test "strict errors carry a position" =
  let strict s () = Seq.iter ignore (Vcd.parse_string ~strict:true s).Vcd.simulation in
  show (strict "$timescale 1ns $end\n");
  show (strict (hdr ^ "#0\n$dumpvars\n0!\n"));
  show (strict (hdr ^ "#0\n0!"));
  [%expect
    {|
    missing $enddefinitions at <string> line 2 col 0
    unterminated dump block at end of input at <string> line 9 col 0
    input does not end with a newline at <string> line 7 col 2 |}]

let%expect_test "a missing file is Io, wherever it is opened" =
  let missing = "data/missing.vcd" in
  show (fun () -> ignore (Vcd.parse_file missing));
  show (fun () -> Vcd.with_file missing ignore);
  show (fun () -> ignore (Vcd_fst.open_file missing));
  show (fun () -> ignore (Vcd_fst.parse_file missing));
  show (fun () -> Vcd_fst.with_file missing ignore);
  show (fun () -> ignore (Vcd_fst.blocks missing));
  show (fun () -> ignore (Vcd_fst.open_at missing ~time:Vcd_types.Timestamp.(of_int64 0L)));
  show (fun () -> ignore (Vcd_fst.detect_file missing));
  show (fun () -> match Vcd_fst.File.open_ missing with Ok _ -> () | Error e -> raise (Vcd.Error.E e));
  [%expect
    {|
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory
    data/missing.vcd: No such file or directory |}]

let%expect_test "an FST error keeps its kind and offset" =
  let cut = In_channel.with_open_bin kitchen (fun ic -> really_input_string ic 400) in
  with_temp cut (fun path ->
      (match Vcd_fst.open_file_result path with
      | Error (`Fst (name, e)) ->
          Printf.printf "name ok %b, kind %s, offset %s\n" (name = path)
            (match Fst.Error.kind e with Truncated -> "truncated" | Malformed -> "malformed" | _ -> "other")
            (match Fst.Error.offset e with
            | Some (`File o) -> Int64.to_string o
            | Some (`Hierarchy o) -> "h" ^ Int64.to_string o
            | None -> "none")
      | _ -> print_endline "unexpected");
      show_path path (fun () -> Seq.iter ignore (Vcd_fst.open_file path).Vcd.simulation));
  [%expect
    {|
    name ok true, kind malformed, offset 330
    <path>: fst: malformed input: block length 3168 (offset 330) |}]

let%expect_test "FST-only functions refuse a VCD" =
  with_temp hdr (fun path ->
      show_path path (fun () -> ignore (Vcd_fst.blocks path));
      show_path path (fun () -> ignore (Vcd_fst.open_at path ~time:(Vcd_types.Timestamp.of_int64 0L)));
      match Vcd_fst.File.open_ path with
      | Error e -> show_path path (fun () -> raise (Vcd.Error.E e))
      | Ok _ -> print_endline "opened");
  [%expect {|
    <path>: not an FST file
    <path>: not an FST file
    <path>: not an FST file |}]

let%expect_test "handle errors" =
  let f = Result.get_ok (Vcd_fst.File.open_ kitchen) in
  let _, a = Vcd_fst.File.stream f in
  let a = match a () with Seq.Cons (_, k) -> k | Nil -> a in
  let _ = Vcd_fst.File.stream f in
  show (fun () -> Seq.iter ignore a);
  Vcd_fst.File.close f;
  show (fun () -> ignore (Vcd_fst.File.stream f));
  [%expect {|
    data/fst2vcd/kitchen.fst: stream superseded
    data/fst2vcd/kitchen.fst: handle closed |}]

let%expect_test "result variants return the eager errors" =
  let pr = function Ok _ -> print_endline "ok" | Error e -> Format.printf "%a\n" Vcd.Error.pp_error e in
  pr (Vcd.parse_string_result "$scope $end");
  pr (Vcd.parse_file_result "data/missing.vcd");
  pr (Vcd_fst.open_file_result "data/missing.fst");
  pr (Result.map ignore (Vcd_fst.blocks_result "data/missing.fst"));
  pr (Vcd.parse_string_result hdr);
  [%expect
    {|
    syntax error at <string> line 1 col 11
    data/missing.vcd: No such file or directory
    data/missing.fst: No such file or directory
    data/missing.fst: No such file or directory
    ok |}]

let%expect_test "E prints through Printexc" =
  (try ignore (Vcd.parse_string "$scope $end") with e -> print_endline (Printexc.to_string e));
  [%expect {| Vcd.Error.E: syntax error at <string> line 1 col 11 |}]
