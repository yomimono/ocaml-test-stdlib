module OrdInt = struct
  type t = int
  let compare (i : int) (j : int) = compare i j
end

module type GENERABLE = sig
  type t
  val gen : t Crowbar.gen
  val pp : t Fmt.t
end
(* for making maps and sets! *)
module String = struct
  type t = string
  let gen = Crowbar.bytes
  let pp = Fmt.string
end
module Int = struct
  type t = int
  let gen = Crowbar.int
  let pp = Fmt.int
end
module Char = struct
  type t = char
  let gen = Crowbar.(Map ([uint8], Char.chr))
  let pp = Fmt.char
end
module Nativeint = struct
  type t = Nativeint.t
  let gen = Crowbar.(Map ([int], Nativeint.of_int))
  let pp f key = Fmt.string f (Nativeint.to_string key)
end
module Uchar = struct
  type t = Uchar.t
  (* stolen (sdolan?) from test_uunf *)
  let gen = Crowbar.(Map ([int32], fun n ->
    let n = Int32.to_int n land 0x1FFFFF in
    try Uchar.of_int n with Invalid_argument _ -> bad_test ()))
  let pp f key = Fmt.int f (Uchar.to_int key)
end
