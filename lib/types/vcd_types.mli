module ID : sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val of_string : string -> t
  val pp : Format.formatter -> t -> unit
end

module Timestamp : sig
  type t

  val equal : t -> t -> bool
  val of_string : string -> t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module Reference : sig
  type t

  val empty : t
  val push : string -> t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val make : string list -> string -> t

  val to_list : t -> string list
  (** Returns components in outermost-to-innermost (display) order. *)

  val of_list : string list -> t
  (** Build a reference from components in outermost-to-innermost (display) order. Inverse of {!to_list}. *)

  val to_string : t -> string
  val of_string : string -> t
end

module Value : sig
  (** 4-state logic value. [B0] = drive-0, [B1] = drive-1, [X] = unknown, [Z] = high-impedance. *)
  type logic = B0 | B1 | X | Z

  val char_of_logic : logic -> char
  val pp_logic : Format.formatter -> logic -> unit

  exception InvalidLogicChar of char

  val char_to_logic_exn : char -> logic

  val int_bits : int
  (** Maximum bit-width stored as a native [int] in the [Int] variant (= 24). *)

  (** Decoded value for a bus/vector signal. *)
  type t =
    | Scalar of logic  (** Single-bit 4-state value. *)
    | Int of int
        (** All-0/1 vector that fits in a portable OCaml [int] (<= [int_bits] = 24 bits). Canonical bit width is in
            {!Vcd_ast.var_decl.size}. *)
    | Int64 of int64
        (** All-0/1 vector of 25..64 bits, held as an [int64]. Canonical bit width is in {!Vcd_ast.var_decl.size}. *)
    | Bytes of bytes  (** All-0/1 vector wider than 64 bits, packed MSB-first into [bytes]. *)
    | Scalars of logic list  (** 4-state vector containing at least one [X] or [Z] bit. *)
    | Other of string  (** 9-state or unrecognised vector (raw bit string kept verbatim). *)
    | Real of float  (** Floating-point real value (from [r…] VCD records). *)

  val to_string : t -> string

  val to_string_hex : int -> t -> string
  (** Like [to_string] but prints multi-bit numeric values ([Int], [Int64], [Bytes]) in hexadecimal with [h] prefix,
      zero-padded to the width implied by [size]. [Scalar], [Scalars], [Other], and [Real] are unchanged. Pass the
      signal's declared bit-width as [size]. *)

  val pp : Format.formatter -> t -> unit
  val zero : t
  val one : t
  val default : t

  val int : int -> t
  (** [int n] constructs an [Int n] value; raises [Invalid_argument] for negative [n]. *)

  val int64 : int64 -> t
  (** [int64 n] returns [Int (Int64.to_int n)] when [n] fits in a native [int], otherwise [Int64 n]. *)

  val bytes : bytes -> t
end
