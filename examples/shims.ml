module OrdInt = struct
  type t = int
  let compare (i : int) (j : int) = compare i j
end
module StringString = struct
  type key = string type value = string
  let key_gen, val_gen = Crowbar.bytes, Crowbar.bytes
  let pp_key, pp_value = Fmt.string, Fmt.string
end
module IntString = struct
  type key = int type value = string
  let key_gen, val_gen = Crowbar.int, Crowbar.bytes
  let pp_key, pp_value = Fmt.int, Fmt.string
end
module CharInt = struct
  type key = char type value = int
  let key_gen, val_gen = Crowbar.(Map ([uint8], Char.chr)), Crowbar.int
  let pp_key, pp_value = Fmt.char, Fmt.int
end
module NativeintString = struct
  type key = Nativeint.t type value = int
  let key_gen, val_gen = Crowbar.(Map ([int], Nativeint.of_int)), Crowbar.int
  let pp_key f key = Fmt.string f (Nativeint.to_string key)
  let pp_value = Fmt.int
end
module UcharString = struct
    type key = Uchar.t type value = string
    (* stolen (sdolan?) from test_uunf *)
    let key_gen = Crowbar.(Map ([int32], fun n ->
      let n = Int32.to_int n land 0x1FFFFF in
      try Uchar.of_int n with Invalid_argument _ -> bad_test ()))
    let val_gen = Crowbar.bytes
    let pp_key f key = Fmt.int f (Uchar.to_int key)
    let pp_value = Fmt.string
end
