open Base

module type GENERABLE = sig
  type t
  val gen : t Crowbar.gen
  val pp : t Fmt.t
  val transform : t -> t (* for map *)
end
(* for making maps and sets! *)
module String = struct
  type t = String.t
  let gen = Crowbar.(map [bytes] String.of_string)
  let pp = Fmt.string
  let transform = function
    | "" -> "\n"
    | n -> String.rev n
end
module Int = struct
  type t = Int.t
  let gen = Crowbar.(map [int] Int.of_int)
  let pp = Fmt.int
  let transform = function
    | n when Int.rem n 2 = 0 -> n + 10
    | n -> n - 10
end
module Char = struct
  type t = Char.t
  let gen = Crowbar.(map [uint8] Char.of_int_exn)
  let pp = Fmt.char
  let transform n =
    if Char.is_uppercase n then Char.lowercase n else Char.uppercase n
end
module Nativeint = struct
  type t = Nativeint.t
  let gen = Crowbar.(map [int] Nativeint.of_int)
  let pp f key = Fmt.string f (Nativeint.to_string key)
  let transform n =
    try Nativeint.succ n
    with Invalid_argument _ -> Nativeint.pred n
end
module Uchar = struct
  type t = Uchar.t
  (* stolen (sdolan?) from test_uunf *)
  let gen = Crowbar.(map [int32] (fun n ->
    let n = Int32.to_int_exn n land 0x1FFFFF in
    Uchar.of_scalar n |> nonetheless))
  let pp f key = Fmt.int f (Uchar.to_scalar key)
  let transform n =
    try Uchar.succ_exn n
    with Invalid_argument _ -> Uchar.pred_exn n
end
module Float = struct
  type t = float
  let gen = Crowbar.float
  let pp = Fmt.float
  let transform f = -. f
end
