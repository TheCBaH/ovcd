module ID = struct
  type t = string

  let equal = String.equal
  let compare = String.compare
  let to_string id = id
  let of_string s = s
  let pp fmt t = Format.pp_print_string fmt t
end

module Timestamp = struct
  type t = int64

  let equal = Int64.equal
  let of_string s = Int64.of_string s
  let compare = Int64.compare
  let pp fmt t = Format.fprintf fmt "%Ld" t
end

module Reference = struct
  type t = string list

  let empty = []
  let push name t = name :: t
  let equal a b = List.equal String.equal a b
  let compare a b = List.compare String.compare a b
  let make scopes name = List.fold_left (fun acc s -> push s acc) (push name empty) scopes
  let to_list v = List.rev v
  let of_list comps = List.fold_left (fun acc s -> push s acc) empty comps
  let to_string v = String.concat "." (List.rev v)
  let of_string s = List.fold_left (fun acc s -> push s acc) empty (String.split_on_char '.' s)
end

module Value = struct
  (** 4-state logic value. [B0] = drive-0, [B1] = drive-1, [X] = unknown, [Z] = high-impedance. *)
  type logic = B0 | B1 | X | Z

  let char_of_logic = function B0 -> '0' | B1 -> '1' | X -> 'x' | Z -> 'z'
  let pp_logic fmt l = Format.pp_print_char fmt (char_of_logic l)

  exception InvalidLogicChar of char

  let char_to_logic_exn = function
    | '0' -> B0
    | '1' -> B1
    | 'x' | 'X' -> X
    | 'z' | 'Z' -> Z
    | c -> raise (InvalidLogicChar c)

  (** Decoded value for a bus/vector signal. *)
  type t =
    | Scalar of logic  (** Single-bit 4-state value. *)
    | Int of int
        (** All-0/1 vector that fits in a portable OCaml [int] (<= 24 bits). Canonical bit width is in {!var_decl.size}.
        *)
    | Int64 of int64
        (** All-0/1 vector of 25..64 bits, held as an [int64]. Canonical bit width is in {!var_decl.size}. *)
    | Bytes of bytes  (** All-0/1 vector wider than 64 bits, packed MSB-first into [bytes]. *)
    | Scalars of logic list  (** 4-state vector containing at least one [X] or [Z] bit. *)
    | Other of string  (** 9-state or unrecognised vector (raw bit string kept verbatim). *)
    | Real of float  (** Floating-point real value (from [r…] VCD records). *)

  let to_string = function
    | Scalar l -> String.make 1 (char_of_logic l)
    | Int n ->
        let buf = Buffer.create 26 in
        Buffer.add_char buf 'b';
        let rec go n =
          if n > 0 then begin
            go (n lsr 1);
            Buffer.add_char buf (if n land 1 = 1 then '1' else '0')
          end
        in
        if n = 0 then Buffer.add_char buf '0' else go n;
        Buffer.contents buf
    | Int64 n ->
        let buf = Buffer.create 66 in
        Buffer.add_char buf 'b';
        let rec go n =
          if n > Int64.zero then begin
            go (Int64.shift_right_logical n 1);
            Buffer.add_char buf (if Int64.logand n 1L = 1L then '1' else '0')
          end
        in
        if n = Int64.zero then Buffer.add_char buf '0' else go n;
        Buffer.contents buf
    | Bytes b ->
        let nbits = Bytes.length b * 8 in
        let buf = Buffer.create (nbits + 1) in
        Buffer.add_char buf 'b';
        for i = 0 to nbits - 1 do
          let byte_idx = i / 8 in
          let bit_pos = 7 - (i mod 8) in
          let bit = (Char.code (Bytes.get b byte_idx) lsr bit_pos) land 1 in
          Buffer.add_char buf (if bit = 1 then '1' else '0')
        done;
        Buffer.contents buf
    | Scalars ls ->
        let buf = Buffer.create (1 + List.length ls) in
        Buffer.add_char buf 'b';
        List.iter (fun l -> Buffer.add_char buf (char_of_logic l)) ls;
        Buffer.contents buf
    | Other s -> "b" ^ s
    | Real f -> string_of_float f

  let to_string_hex size = function
    | Scalar l -> String.make 1 (char_of_logic l)
    | Int n -> if size < 2 then to_string (Int n) else Printf.sprintf "h%0*x" ((size + 3) / 4) n
    | Int64 n ->
        let hex = Printf.sprintf "%Lx" n in
        let pad = max 0 (((size + 3) / 4) - String.length hex) in
        "h" ^ String.make pad '0' ^ hex
    | Bytes b ->
        let buf = Buffer.create ((Bytes.length b * 2) + 1) in
        Buffer.add_char buf 'h';
        Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
        Buffer.contents buf
    | (Scalars _ | Other _) as v -> to_string v
    | Real f -> string_of_float f

  let zero = Int 0
  let one = Int 1
  let default = zero

  let int n =
    match n with
    | 0 -> zero
    | 1 -> one
    | n when n > 1 -> Int n
    | _ -> invalid_arg "Value.int: negative integers not supported"

  let int64 n = if n >= Int64.zero && n <= Int64.of_int max_int then int (Int64.to_int n) else Int64 n
  let bytes b = Bytes b
  let pp fmt v = Format.pp_print_string fmt (to_string v)
end
