let vcd =
  {|$timescale 1ns $end
$scope module top $end
$var wire 4 ! data [3:0] $end
$scope module sub $end
$var reg 1 " clk $end
$upscope $end
$upscope $end
$enddefinitions $end
|}

let signals () = List.of_seq (Vcd_signals.of_header (Vcd.parse_string vcd).header)

let%test "declaration order and fields" =
  let open Vcd_signals in
  match signals () with
  | [ a; b ] ->
      a.path = "top.data" && a.width = 4 && a.var_type = "wire" && a.index = Some "[3:0]"
      && a.scope_types = [ "module" ] && b.path = "top.sub.clk"
      && b.parts = [ "top"; "sub"; "clk" ]
      && b.scope_types = [ "module"; "module" ]
      && b.var_type = "reg" && b.index = None
  | _ -> false

let%test "a line decodes to the signal it encodes" =
  List.for_all (fun s -> Vcd_signals.of_line (Vcd_signals.to_line s) = Ok s) (signals ())

let%test "the decoder refuses an object that is not a signal" = Result.is_error (Vcd_signals.of_line "{}")

let one name =
  List.hd
    (List.of_seq
       (Vcd_signals.of_header
          (Vcd.parse_string
             (Printf.sprintf "$scope module top $end $var wire 1 ! %s $end $upscope $end $enddefinitions $end\n" name))
            .header))

let%expect_test "valid multibyte UTF-8 round-trips exactly" =
  List.iter
    (fun name ->
      let s = one name in
      match Vcd_signals.of_line (Vcd_signals.to_line s) with
      | Ok s' -> Printf.printf "%b %b\n" (s = s') (List.nth s'.parts 1 = name)
      | Error e -> print_endline e)
    [ "caf\xc3\xa9"; "\xe2\x82\xac"; "\xf0\x9f\x94\xa5x"; "a\xc3\xa9\xe2\x82\xac" ];
  [%expect {|
    true true
    true true
    true true
    true true |}]

let%expect_test "malformed UTF-8 is an error naming the signal" =
  List.iter
    (fun name ->
      match Vcd.Error.catch (fun () -> Vcd_signals.to_line (one name)) with
      | Ok l -> Printf.printf "line %S\n" l
      | Error e -> print_endline (Vcd.Error.to_string e))
    [ "caf\xe9"; "\xc3"; "\xc0\xaf"; "\xed\xa0\x80"; "ok\xff" ];
  [%expect
    {|
    signal "top.caf\233": name is not valid UTF-8
    signal "top.\195": name is not valid UTF-8
    signal "top.\192\175": name is not valid UTF-8
    signal "top.\237\160\128": name is not valid UTF-8
    signal "top.ok\255": name is not valid UTF-8 |}]
