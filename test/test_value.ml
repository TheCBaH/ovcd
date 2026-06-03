open Vcd_types
open Value

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let exn f =
  try ignore (f ())
  with Invalid_argument msg ->
    print_string msg;
    print_char '\n'

(* ------------------------------------------------------------------ *)
(*  get_int_exn                                                        *)
(* ------------------------------------------------------------------ *)

let%expect_test "get_int_exn: scalar" =
  Printf.printf "%d\n" (Value.get_int_exn (Scalar B0));
  Printf.printf "%d\n" (Value.get_int_exn (Scalar B1));
  [%expect {|
    0
    1
  |}]

let%expect_test "get_int_exn: Int and Int64-that-fits" =
  Printf.printf "%d\n" (Value.get_int_exn Value.zero);
  Printf.printf "%d\n" (Value.get_int_exn (Int 0xABCD));
  Printf.printf "%d\n" (Value.get_int_exn (Int64 100L));
  [%expect {|
    0
    43981
    100
  |}]

let%expect_test "get_int_exn: errors" =
  exn (fun () -> Value.get_int_exn (Scalar X));
  exn (fun () -> Value.get_int_exn (Scalar Z));
  exn (fun () -> Value.get_int_exn (Int64 Int64.max_int));
  exn (fun () -> Value.get_int_exn (Bytes (Bytes.make 9 '\x00')));
  exn (fun () -> Value.get_int_exn (Scalars [ B0; X ]));
  exn (fun () -> Value.get_int_exn (Other "0102"));
  exn (fun () -> Value.get_int_exn (Real 1.0));
  [%expect
    {|
    Value.get_int_exn: X/Z value
    Value.get_int_exn: X/Z value
    Value.get_int_exn: value out of int range
    Value.get_int_exn: value too wide for int
    Value.get_int_exn: X/Z value
    Value.get_int_exn: 9-state or unrecognised value
    Value.get_int_exn: real value
  |}]

(* ------------------------------------------------------------------ *)
(*  get_int64_exn                                                      *)
(* ------------------------------------------------------------------ *)

let%expect_test "get_int64_exn: all numeric variants" =
  Printf.printf "%Ld\n" (Value.get_int64_exn (Scalar B0));
  Printf.printf "%Ld\n" (Value.get_int64_exn (Scalar B1));
  Printf.printf "%Ld\n" (Value.get_int64_exn (Int 255));
  Printf.printf "%Ld\n" (Value.get_int64_exn (Int64 0xDEADBEEF00000000L));
  [%expect {|
    0
    1
    255
    -2401053092612145152
  |}]

let%expect_test "get_int64_exn: errors" =
  exn (fun () -> Value.get_int64_exn (Scalar Z));
  exn (fun () -> Value.get_int64_exn (Bytes (Bytes.make 9 '\x00')));
  exn (fun () -> Value.get_int64_exn (Real 3.14));
  [%expect
    {|
    Value.get_int64_exn: X/Z value
    Value.get_int64_exn: value too wide for int64
    Value.get_int64_exn: real value
  |}]

(* ------------------------------------------------------------------ *)
(*  get_byte_exn                                                       *)
(* ------------------------------------------------------------------ *)

let%expect_test "get_byte_exn: Int – LSB-first, zero fill beyond payload" =
  let v = Int 0xABCD in
  Printf.printf "%02x\n" (Value.get_byte_exn v 0);
  Printf.printf "%02x\n" (Value.get_byte_exn v 1);
  Printf.printf "%02x\n" (Value.get_byte_exn v 2);
  [%expect {|
    cd
    ab
    00
  |}]

let%expect_test "get_byte_exn: Int64" =
  (* 0x0000DEADBEEF: byte 0 = 0xEF, 1 = 0xBE, 2 = 0xAD, 3 = 0xDE, 4+ = 0 *)
  let v = Int64 0x0000DEADBEEFL in
  Printf.printf "%02x\n" (Value.get_byte_exn v 0);
  Printf.printf "%02x\n" (Value.get_byte_exn v 1);
  Printf.printf "%02x\n" (Value.get_byte_exn v 2);
  Printf.printf "%02x\n" (Value.get_byte_exn v 3);
  Printf.printf "%02x\n" (Value.get_byte_exn v 8);
  [%expect {|
    ef
    be
    ad
    de
    00
  |}]

let%expect_test "get_byte_exn: Bytes MSB-first storage, LSB-first indexing" =
  (* 3-byte payload: 0x01 0x02 0x03 → value = 0x010203
     byte_idx 0 = LSB = 0x03, 1 = 0x02, 2 = 0x01, 3 = zero fill *)
  let v = Bytes (Bytes.of_string "\x01\x02\x03") in
  Printf.printf "%02x\n" (Value.get_byte_exn v 0);
  Printf.printf "%02x\n" (Value.get_byte_exn v 1);
  Printf.printf "%02x\n" (Value.get_byte_exn v 2);
  Printf.printf "%02x\n" (Value.get_byte_exn v 3);
  [%expect {|
    03
    02
    01
    00
  |}]

let%expect_test "get_byte_exn: Scalar" =
  Printf.printf "%d\n" (Value.get_byte_exn (Scalar B0) 0);
  Printf.printf "%d\n" (Value.get_byte_exn (Scalar B1) 0);
  Printf.printf "%d\n" (Value.get_byte_exn (Scalar B1) 1);
  [%expect {|
    0
    1
    0
  |}]

let%expect_test "get_byte_exn: errors" =
  exn (fun () -> Value.get_byte_exn (Scalar X) 0);
  exn (fun () -> Value.get_byte_exn (Real 1.0) 0);
  exn (fun () -> Value.get_byte_exn (Int 0) (-1));
  [%expect
    {|
    Value.get_byte_exn: X/Z value
    Value.get_byte_exn: real value
    Value.get_byte_exn: negative offset
  |}]

(* ------------------------------------------------------------------ *)
(*  get_bits_exn                                                       *)
(* ------------------------------------------------------------------ *)

let%expect_test "get_bits_exn: single bit" =
  let v = Int 0b1010 in
  Printf.printf "%d\n" (Value.get_bits_exn v ~lo:0 ~hi:0);
  Printf.printf "%d\n" (Value.get_bits_exn v ~lo:1 ~hi:1);
  Printf.printf "%d\n" (Value.get_bits_exn v ~lo:2 ~hi:2);
  Printf.printf "%d\n" (Value.get_bits_exn v ~lo:3 ~hi:3);
  [%expect {|
    0
    1
    0
    1
  |}]

let%expect_test "get_bits_exn: byte-aligned slice" =
  let v = Int 0xABCD in
  Printf.printf "%02x\n" (Value.get_bits_exn v ~lo:0 ~hi:7);
  Printf.printf "%02x\n" (Value.get_bits_exn v ~lo:8 ~hi:15);
  [%expect {|
    cd
    ab
  |}]

let%expect_test "get_bits_exn: sub-byte unaligned" =
  (* 0b10110101 = 181; bits 5:2 = 0b1101 = 13 *)
  let v = Int 0b10110101 in
  Printf.printf "%d\n" (Value.get_bits_exn v ~lo:2 ~hi:5);
  [%expect {| 13 |}]

let%expect_test "get_bits_exn: cross-byte boundary" =
  (* 0xABCD; bits 15:4 = 0xABC *)
  let v = Int 0xABCD in
  Printf.printf "%03x\n" (Value.get_bits_exn v ~lo:4 ~hi:15);
  [%expect {| abc |}]

let%expect_test "get_bits_exn: zero fill beyond payload" =
  (* Int 0xFF = bits 7:0 = 0xFF; bits 13:6 = 0b00000011 = 3 *)
  let v = Int 0xFF in
  Printf.printf "%d\n" (Value.get_bits_exn v ~lo:6 ~hi:13);
  [%expect {| 3 |}]

let%expect_test "get_bits_exn: Bytes value" =
  (* 0x010203 stored as "\x01\x02\x03"; bits 15:8 = 0x02 *)
  let v = Bytes (Bytes.of_string "\x01\x02\x03") in
  Printf.printf "%02x\n" (Value.get_bits_exn v ~lo:8 ~hi:15);
  Printf.printf "%02x\n" (Value.get_bits_exn v ~lo:16 ~hi:23);
  Printf.printf "%02x\n" (Value.get_bits_exn v ~lo:24 ~hi:31);
  [%expect {|
    02
    01
    00
  |}]

let%expect_test "get_bits_exn: int_bits limit" =
  (* width = int_bits = 24 is the maximum allowed *)
  Printf.printf "%x\n" (Value.get_bits_exn (Int 0xFFFFFF) ~lo:0 ~hi:(Value.int_bits - 1));
  exn (fun () -> Value.get_bits_exn (Int 0) ~lo:0 ~hi:Value.int_bits);
  [%expect {|
    ffffff
    Value.get_bits_exn: range too wide for int
  |}]

let%expect_test "get_bits_exn: errors" =
  exn (fun () -> Value.get_bits_exn (Int 0) ~lo:(-1) ~hi:0);
  exn (fun () -> Value.get_bits_exn (Int 0) ~lo:5 ~hi:3);
  exn (fun () -> Value.get_bits_exn (Scalar X) ~lo:0 ~hi:0);
  [%expect
    {|
    Value.get_bits_exn: invalid range
    Value.get_bits_exn: invalid range
    Value.get_byte_exn: X/Z value
  |}]

(* ------------------------------------------------------------------ *)
(*  get_bits64_exn                                                     *)
(* ------------------------------------------------------------------ *)

let%expect_test "get_bits64_exn: 64-bit full range from Int64" =
  (* Extract all 64 bits from a large Int64 value *)
  let v = Int64 0xDEADBEEF12345678L in
  Printf.printf "%Lx\n" (Value.get_bits64_exn v ~lo:0 ~hi:63);
  [%expect {| deadbeef12345678 |}]

let%expect_test "get_bits64_exn: cross-byte unaligned 40-bit slice from Bytes" =
  (* 6-byte payload 0x112233445566; bits 39:0 = 0x2233445566, bits 47:8 = 0x1122334455 *)
  let v = Bytes (Bytes.of_string "\x11\x22\x33\x44\x55\x66") in
  Printf.printf "%Lx\n" (Value.get_bits64_exn v ~lo:0 ~hi:39);
  Printf.printf "%Lx\n" (Value.get_bits64_exn v ~lo:8 ~hi:47);
  [%expect {|
    2233445566
    1122334455
  |}]

let%expect_test "get_bits64_exn: zero fill" =
  let v = Int 0xFF in
  Printf.printf "%Ld\n" (Value.get_bits64_exn v ~lo:4 ~hi:67);
  [%expect {| 15 |}]

let%expect_test "get_bits64_exn: errors" =
  exn (fun () -> Value.get_bits64_exn (Int 0) ~lo:0 ~hi:64);
  exn (fun () -> Value.get_bits64_exn (Real 1.0) ~lo:0 ~hi:7);
  [%expect {|
    Value.get_bits64_exn: range too wide for int64
    Value.get_byte_exn: real value
  |}]

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-sonnet-4-6"]
[@@@ai_provider "Anthropic"]
