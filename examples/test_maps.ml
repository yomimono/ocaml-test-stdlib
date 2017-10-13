module type GENERATOR = sig
  type key
  type value
  val key_gen : key Crowbar.gen
  val val_gen : value Crowbar.gen
end

module Map_tester(Map : Map.S)(G: GENERATOR with type key = Map.key) = struct

  include G

  let pair = Crowbar.(Map ([G.key_gen; G.val_gen], fun x y -> x, y))

  let map =
    Crowbar.(Choose [
      Const Map.empty;
      Map ([G.key_gen; G.val_gen], Map.singleton);
      Map ([List pair], fun items ->
          List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty items);
    ])

  let check_bounds map =
    Crowbar.check @@ try
      match Map.min_binding map, Map.max_binding map with
      | (min, _) , (max, _) when compare max min = 1 -> true
      | (min, _) , (max, _) when compare max min = -1 -> false
      | (min, _) , (max, _) ->
        Map.for_all (fun k _ -> compare k min = 0) map
    with
    | Not_found -> 0 = Map.cardinal map

  let nondestructive_binding map (k, v) =
    Crowbar.check @@
    try
      match Map.mem k map with
      | false -> (* inserting should always get us the element *)
        0 = v - (Map.find k @@ Map.update k (function None -> Some v | e -> e) map)
      | true -> (* inserting should always return the previous value *)
        let v' = Map.find k map in
        0 = v' - (Map.find k @@ Map.update k (function None -> Some v | e -> e) map)
    with
    | Not_found -> false

  let destructive_binding map (k, v) =
    Crowbar.check @@
    try
      (* inserting should always get us the element *)
      0 = v - (Map.find k @@ Map.update k (fun _ -> Some v) map)
    with
    | Not_found -> false

  let replace map (k, v) =
    Crowbar.check @@
    let replace k v map =
      Map.update k (function None -> None | Some _ -> Some v) map
    in
    match Map.mem k map with
    | true ->
      0 = v - (Map.find k @@ replace k v map)
    | false ->
      0 = compare None (Map.find_opt k @@ replace k v map)

  (* I don't know why this is important, but it's in the unit tests, so let's
     include it *)
  let delete_extant_bind_new map (k, v) =
    Crowbar.check @@
    let transform k v map = Map.update k (function None -> Some v | Some _ ->
        None) map
    in
    match Map.mem k map with
    | false -> (* our new binding should be there after transformation *)
      0 = v - (Map.find k @@ transform k v map)
    | true -> 
      0 = compare None (Map.find_opt k @@ transform k v map)

end

module IntMap = Map.Make (struct
  type t = int
  let compare (i : int) (j : int) = compare i j
end)
module IntGen = struct
  type key = int
  type value = int
  let key_gen, val_gen = Crowbar.int, Crowbar.int
end
module IntTester = Map_tester(IntMap)(IntGen)

let () =
  let module Tester = IntTester in
  Crowbar.add_test ~name:"max_binding = min_binding implies all elements are equal"
    Crowbar.[Tester.map] Tester.check_bounds;
  Crowbar.add_test ~name:"non-destructive updates never shadow existing bindings"
    Crowbar.[Tester.map; Tester.pair] Tester.nondestructive_binding;
  Crowbar.add_test ~name:"destructive updates always shadow existing bindings"
    Crowbar.[Tester.map; Tester.pair] Tester.destructive_binding;
  Crowbar.add_test ~name:"replacing does not create new bindings"
    Crowbar.[Tester.map; Tester.pair] Tester.replace;
  Crowbar.add_test ~name:"delete-if-present transformation works"
    Crowbar.[Tester.map; Tester.pair] Tester.delete_extant_bind_new;
