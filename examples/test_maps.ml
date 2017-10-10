module type Generable = sig
  include Map.OrderedType
  val generate : t Crowbar.gen
end

module StringGen = struct
  include String
  let generate = Crowbar.bytes
end

module IntGen = struct
  type t = int
  let compare p q = p - q
  let generate = Crowbar.int
end

let key_module : (module Generable) Crowbar.gen =
  Crowbar.(Choose [
      Const (module IntGen);
      Const (module StringGen);
    ])

let check_empty g =
  let module G = (val g : Generable) in
  let module M = Map.Make(G) in
  Crowbar.check @@ M.is_empty M.empty

let () =
  Crowbar.add_test ~name:"empty maps are empty"
    Crowbar.[key_module] check_empty
