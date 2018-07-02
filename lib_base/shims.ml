open Base

module type Comparably_buildable = sig
  type t
  include Comparator.S with type t := t
  val sexp_of_t : t -> Sexp.t
end

module type GENERABLE = sig
  type t
  val gen : t Crowbar.gen
  val sexp_of_t : t -> Sexp.t
  val transform : t -> t (* for map *)
end
(* for making maps and sets! *)
module String = struct
  type t = String.t
  let sexp_of_t = String.sexp_of_t
  let gen = Crowbar.(map [bytes] String.of_string)
  let transform = function
    | "" -> "\n"
    | n -> String.rev n
end
module Int = struct
  type t = Int.t
  let sexp_of_t = Int.sexp_of_t
  let gen = Crowbar.(map [int] Int.of_int)
  let transform = function
    | n when Int.rem n 2 = 0 -> n + 10
    | n -> n - 10
end
module Char = struct
  type t = Char.t
  let sexp_of_t = Char.sexp_of_t
  let gen = Crowbar.(map [uint8] Char.of_int_exn)
  let transform n =
    if Char.is_uppercase n then Char.lowercase n else Char.uppercase n
end
module Nativeint = struct
  type t = Nativeint.t
  let sexp_of_t = Nativeint.sexp_of_t
  let gen = Crowbar.(map [int] Nativeint.of_int)
  let transform n =
    try Nativeint.succ n
    with Invalid_argument _ -> Nativeint.pred n
end
module Uchar = struct
  type t = Uchar.t
  let sexp_of_t = Uchar.sexp_of_t
  (* stolen (sdolan?) from test_uunf *)
  let gen = Crowbar.(map [int32] (fun n ->
    let n = Int32.to_int_exn n land 0x1FFFFF in
    Uchar.of_scalar n |> nonetheless))
  let transform n =
    try Uchar.succ_exn n
    with Invalid_argument _ -> Uchar.pred_exn n
end
module Float = struct
  type t = Float.t
  let sexp_of_t = Float.sexp_of_t
  let gen = Crowbar.float
  let transform f = -. f
end
