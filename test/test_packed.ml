open Vcd_types
open Value

let hex b = String.concat "" (List.init (Bytes.length b) (fun i -> Printf.sprintf "%02x" (Bytes.get_uint8 b i)))

(* "bits/unknown" in little-endian bytes, or the error. *)
let show ~width v =
  match Packed.of_value ~width v with
  | Ok { bits; unknown } -> hex bits ^ "/" ^ hex unknown
  | Error (Not_a_vector _) -> "not a vector"
  | Error (Too_wide { width; bytes }) -> Printf.sprintf "too wide: %d bytes in %d bits" bytes width

let%expect_test "Int and Int64 are taken modulo 2^width" =
  List.iter
    (fun (w, v) -> Printf.printf "%d %s\n" w (show ~width:w v))
    [
      (8, Int 5);
      (3, Int 13);
      (24, Int 0xabcdef);
      (64, Int64 0x0102030405060708L);
      (72, Int64 0x0102030405060708L);
      (12, Int64 0xfffL);
      (4, Int64 (-1L));
    ];
  [%expect
    {|
    8 05/00
    3 05/00
    24 efcdab/000000
    64 0807060504030201/0000000000000000
    72 080706050403020100/000000000000000000
    12 ff0f/0000
    4 0f/00
  |}]

let%expect_test "Bytes are big-endian; a bit at or above the width is too wide" =
  List.iter
    (fun (w, b) -> Printf.printf "%d %s\n" w (show ~width:w (Bytes (Bytes.of_string b))))
    [
      (16, "\x01\x02");
      (9, "\x01\x02");
      (8, "\x01\x02");
      (8, "\x00\x00\x05");
      (13, "\x1f\xff");
      (12, "\x1f\xff");
      (24, "\x00\x01");
      (72, "\x01\x00\x00\x00\x00\x00\x00\x00\x02");
    ];
  [%expect
    {|
    16 0201/0000
    9 0201/0000
    8 too wide: 2 bytes in 8 bits
    8 05/00
    13 ff1f/0000
    12 too wide: 2 bytes in 12 bits
    24 010000/000000
    72 020000000000000001/000000000000000000
  |}]

let%expect_test "all nine states" =
  (* From the left: 0 1 x z u w l h -, so bit 8 is the 0 and bit 0 the '-'. *)
  print_endline (show ~width:9 (Scalars [ B0; B1; X; Z; U; W; L; H; Dont_care ]));
  [%expect {| 8200/7f00 |}]

let%expect_test "extension" =
  List.iter
    (fun (w, v) -> Printf.printf "%d %s\n" w (show ~width:w v))
    [
      (4, Scalars [ X; B1 ]);
      (4, Scalars [ Z; B0 ]);
      (3, Scalars [ U; B1 ]);
      (3, Scalars [ B1; X ]);
      (2, Scalar H);
      (1, Scalar X);
      (1, Scalar B1);
      (2, Scalars [ B1; B0; B1 ]);
    ];
  [%expect {|
    4 01/0e
    4 00/0e
    3 01/02
    3 02/01
    2 01/01
    1 00/01
    1 01/00
    2 01/00
  |}]

let%expect_test "values that are not vectors" =
  List.iter (fun v -> print_endline (show ~width:8 v)) [ Real 1.5; Other "?"; Port "D0 0 0"; String "abc" ];
  [%expect {|
    not a vector
    not a vector
    not a vector
    not a vector
  |}]
