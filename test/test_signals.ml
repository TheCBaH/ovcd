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
