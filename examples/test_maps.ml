module type GENERATOR = sig
  type key
  type value
  val key_gen : key Crowbar.gen
  val val_gen : value Crowbar.gen
end

module Map_tester(KeyOrder: Map.OrderedType)(ValueOrder: Map.OrderedType)
    (G: GENERATOR with type key = KeyOrder.t and type value = ValueOrder.t) = struct

  module Map = Map.Make(KeyOrder)

  let pair : (G.key * G.value) Crowbar.gen 
    = Crowbar.(Map ([G.key_gen; G.val_gen], fun x y -> x, y))

  let largest val1 val2 =
    match ValueOrder.compare val1 val2 with
    | x when x < 0 -> Some val2
    | 0 -> Some val2
    | _ -> Some val1

  let rec map =
    Crowbar.(Choose [
        Const Map.empty;
        Map ([G.key_gen; G.val_gen], Map.singleton);
        Map ([List pair], fun items ->
            List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty items);
        Map ([map; map], fun m1 m2 ->
            Map.union (fun _key val1 val2 -> largest val1 val2) m1 m2);
      ])

  let check_bounds map =
    Crowbar.check @@ try
      match Map.min_binding map, Map.max_binding map with
      | (min, _) , (max, _) when KeyOrder.compare max min = 1 -> true
      | (min, _) , (max, _) when KeyOrder.compare max min = -1 -> false
      | (min, _) , (max, _) ->
        Map.for_all (fun k _ -> KeyOrder.compare k min = 0) map
    with
    | Not_found -> 0 = Map.cardinal map

  module Equality = struct

    let check_add m (k, v) =
      Crowbar.check @@
      match Map.find_opt k m with
      | Some v' when v == v -> m == Map.add k v m
      | Some other_v -> Crowbar.bad_test ()
      | None ->
        let m = Map.add k v m in
        m == Map.add k v m

    let check_remove m k =
      Crowbar.check @@
        match Map.mem k m with
        | false ->
          (* calling [remove k m] when [k] is not bound in [m] is claimed to
             always return a map physically equal to [m] since 4.03.0 *)
          (Map.remove k m) == m
        | true ->
          let without_k = Map.remove k m in
          ((Map.remove k without_k) == without_k) &&
          (* also, without_k should not be equal (in any sense!)
             to the map with k in it *)
          (0 <> Map.compare ValueOrder.compare m without_k)

    let check_filter m =
      Crowbar.check (Map.filter (fun _ _ -> true) m == m)

    let check_choose m =
      let also_m = m in
      Crowbar.check @@
      match Map.choose m, Map.choose also_m with
      | (k1, v1), (k2, v2) -> (0 = KeyOrder.compare k1 k2 &&
                               0 = ValueOrder.compare v1 v2)
      | exception Not_found -> Crowbar.bad_test ()
  
  end

  module Update = struct

    let nondestructive_binding map (k, v) =
      Crowbar.check @@ try
        match Map.mem k map with
        | false -> (* inserting should always get us the element *)
          0 = ValueOrder.compare v
            (Map.find k @@ Map.update k (function None -> Some v | e -> e) map)
        | true -> (* inserting should always return the previous value *)
          let v' = Map.find k map in
          0 = ValueOrder.compare v'
            (Map.find k @@ Map.update k (function None -> Some v | e -> e) map)
      with
      | Not_found -> false

    let destructive_binding map (k, v) =
      Crowbar.check @@
      try
        (* inserting should always get us the element *)
        0 = ValueOrder.compare v (Map.find k @@ Map.update k (fun _ -> Some v) map)
      with
      | Not_found -> false

    let replace map (k, v) =
      Crowbar.check @@
      let replace k v map =
        Map.update k (function None -> None | Some _ -> Some v) map
      in
      match Map.mem k map with
      | true ->
        0 = ValueOrder.compare v (Map.find k @@ replace k v map)
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
        0 = ValueOrder.compare v (Map.find k @@ transform k v map)
      | true -> 
        0 = compare None (Map.find_opt k @@ transform k v map)
  end

  module Union = struct

    let union_largest m1 m2 =
      let unioned = Map.union (fun _key val1 val2 -> largest val1 val2) m1 m2 in
      let merged = Map.merge (fun _key x y -> match x, y with
          | None, None -> None
          | Some v, None | None, Some v -> Some v
          | Some x, Some y -> largest x y) m1 m2
      in
      Crowbar.check @@ Map.equal (fun x y -> 0 = ValueOrder.compare x y) merged unioned
  end


  let add_tests () =
    Crowbar.add_test ~name:"max_binding = min_binding implies all elements are equal"
      Crowbar.[map] check_bounds;
    Equality.(
    Crowbar.add_test ~name:"removing a key that isn't bound preserves physical \
                            equality" Crowbar.[map; G.key_gen] check_remove;
    Crowbar.add_test ~name:"filtering which keeps all elements retains physical \
                            equality" Crowbar.[map] check_filter;
    Crowbar.add_test ~name:"choose gets the same binding for same maps"
      Crowbar.[map] check_choose;
    Crowbar.add_test ~name:"add of a physically equal element gets a physically \
                            equal map" Crowbar.[map; pair] check_add;
    );
    Update.(
      Crowbar.add_test ~name:"destructive updates always shadow existing bindings"
        Crowbar.[map; pair] destructive_binding;
      Crowbar.add_test ~name:"non-destructive updates never shadow existing bindings"
        Crowbar.[map; pair] nondestructive_binding;
      Crowbar.add_test ~name:"replacing does not create new bindings"
        Crowbar.[map; pair] replace;
      Crowbar.add_test ~name:"delete-if-present transformation works"
        Crowbar.[map; pair] delete_extant_bind_new;
    );
    Union.(
      Crowbar.add_test ~name:"Map.union is special case of Map.merge as claimed"
        Crowbar.[map; map] union_largest
    );

end

module OrdInt = struct
  type t = int
  let compare (i : int) (j : int) = compare i j
end
module IntGen = struct
  type key = int
  type value = int
  let key_gen, val_gen = Crowbar.int, Crowbar.int
end
module IntTester = Map_tester(OrdInt)(OrdInt)(IntGen)
module StringGen = struct
  type key = string
  type value = string
  let key_gen, val_gen = Crowbar.bytes, Crowbar.bytes
end
module StringTester = Map_tester(String)(String)(StringGen)
module IntStringTester = Map_tester(OrdInt)(String)(struct
    type key = int type value = string
    let key_gen, val_gen = Crowbar.int, Crowbar.bytes
  end)

let () =
  StringTester.add_tests ();
  IntTester.add_tests ();
  IntStringTester.add_tests ();
