module type GENERATOR = sig
  type key
  type value
  val key_gen : key Crowbar.gen
  val val_gen : value Crowbar.gen
  val pp_key : key Fmt.t
  val pp_value : value Fmt.t
  val key_transform : key -> key
  val value_transform : value -> value
end

module Map_tester(KeyOrder: Map.OrderedType)(ValueOrder: Map.OrderedType)
    (G: GENERATOR with type key = KeyOrder.t and type value = ValueOrder.t) = struct

  module Map = Map.Make(KeyOrder)

  let pair : (G.key * G.value) Crowbar.gen
    = Crowbar.(Map ([G.key_gen; G.val_gen], fun x y -> x, y))

  let pp_map f m =
    let pairsep = Fmt.(const string " -> ") in
    let listsep = Fmt.(const string " | ") in
    let pp_pairmap = Fmt.list ~sep:listsep
        (Fmt.pair ~sep:pairsep G.pp_key G.pp_value) in
    pp_pairmap f (Map.bindings m)

  let largest val1 val2 =
    match ValueOrder.compare val1 val2 with
    | x when x < 0 -> Some val2
    | 0 -> Some val2
    | _ -> Some val1

  let map_eq =
    Crowbar.check_eq
      ~eq:(Map.equal (fun x y -> 0 = ValueOrder.compare x y))
      ~cmp:(Map.compare ValueOrder.compare)
      ~pp:pp_map

  let disjunction =
    Map.merge (fun _key val1 val2 -> match val1, val2 with
        | None, None -> None
        | Some v, None | None, Some v -> Some v
        | Some _, Some _ -> None)

  let rec map =
    Crowbar.(Choose [
        Const Map.empty;
        Map ([G.key_gen; G.val_gen], Map.singleton);
        Map ([List pair], fun items ->
            List.fold_left (fun m (k, v) -> Map.add k v m) Map.empty items);
        Map ([map; map], fun m1 m2 ->
            Map.union (fun _key val1 val2 -> largest val1 val2) m1 m2);
        Map ([map; G.key_gen], fun m k -> Map.remove k m);
        Map ([map; pair], fun m (k, v) -> Map.add k v m);
        Map ([map; map], disjunction); (* Map.merge *)
        Map ([map; List pair], fun map l ->
             Map.filter (fun k v -> try
                            0 = ValueOrder.compare v @@ List.assoc k l
                          with Not_found -> false
                        ) map);
        Map ([map; List pair], fun map l ->
             snd @@ Map.partition (fun k v -> try
                            0 = ValueOrder.compare v @@ List.assoc k l
                          with Not_found -> false
                                  ) map);
        Map ([map; G.key_gen], fun m k ->
            (* we could test whether this is equivalent to Map.remove k m *)
            let l, _, r = Map.split k m in
            disjunction r l);
        Map ([map], fun m -> Map.map G.value_transform m);
        Map ([map], fun m -> Map.mapi (fun _ a -> G.value_transform a) m);
      ])

  let check_bounds map =
    match Map.min_binding map, Map.max_binding map with
    | (k1, _v1) , (k2, v2) when KeyOrder.compare k1 k2 = 0 ->
      (* this only makes sense for the singleton map *)
      map_eq map @@ Map.singleton k1 v2
    | (min, _) , (max, _) -> KeyOrder.compare max min > 0 |> Crowbar.check
    | exception Not_found -> map_eq Map.empty map

  module Equality = struct

    let phy_eq = Crowbar.check_eq
      ~eq:(fun x y -> x == y)
      ~cmp:(Map.compare ValueOrder.compare)
      ~pp:pp_map

    let check_add m (k, v) =
      let add_check m (k, v) =
        let m_with_v = Map.add k v m in
        phy_eq m_with_v @@ Map.add k v m_with_v
      in
      try
        if Map.find k m == v then phy_eq m @@ Map.add k v m
        else add_check m (k, v)
      with
      | Not_found -> add_check m (k, v)

    let check_remove m k =
        match Map.mem k m with
        | false ->
          (* calling [remove k m] when [k] is not bound in [m] is claimed to
             always return a map physically equal to [m] since 4.03.0 *)
          phy_eq m @@ Map.remove k m
        | true ->
          let without_k = Map.remove k m in
          phy_eq without_k @@ Map.remove k without_k

    let check_filter m =
      phy_eq m @@ Map.filter (fun _ _ -> true) m

    let check_choose m =
      let also_m = m in
      Crowbar.check @@
      match Map.choose m, Map.choose also_m with
      | (k1, v1), (k2, v2) -> (0 = KeyOrder.compare k1 k2 &&
                               0 = ValueOrder.compare v1 v2)
      | exception Not_found -> Crowbar.bad_test ()

  end
  module Union = struct

    let union_largest m1 m2 =
      let unioned = Map.union (fun _key val1 val2 -> largest val1 val2) m1 m2 in
      let merged = Map.merge (fun _key x y -> match x, y with
          | None, None -> None
          | Some v, None | None, Some v -> Some v
          | Some x, Some y -> largest x y) m1 m2
      in
      map_eq merged unioned
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
    Union.(
      Crowbar.add_test ~name:"Map.union is special case of Map.merge as claimed"
        Crowbar.[map; map] union_largest
    );

end

module Make_generator(K: Shims.GENERABLE)(V: Shims.GENERABLE) = struct
  type key = K.t
  type value = V.t
  let key_gen, pp_key = K.gen, K.pp
  let val_gen, pp_value = V.gen, V.pp
  let key_transform, value_transform = K.transform, V.transform
end
module StringString = Make_generator(Shims.String)(Shims.String)
module IntString = Make_generator(Shims.Int)(Shims.String)
module FloatString = Make_generator(Shims.Float)(Shims.String)
module CharInt = Make_generator(Shims.Char)(Shims.Int)
module NativeintInt = Make_generator(Shims.Nativeint)(Shims.Int)
module UcharString = Make_generator(Shims.Uchar)(Shims.String)

module StringTester = Map_tester(String)(String)(StringString)
module IntStringTester = Map_tester(Shims.OrdInt)(String)(IntString)
module FloatStringTester = Map_tester(Shims.OrdFloat)(String)(FloatString)
module CharIntTester = Map_tester(Char)(Shims.OrdInt)(CharInt)
module NativeIntTester = Map_tester(Nativeint)(Shims.OrdInt)(NativeintInt)
module UcharStringTester = Map_tester(Uchar)(String)(UcharString)
