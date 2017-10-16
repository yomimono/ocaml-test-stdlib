module type GENERATOR = sig
  type key
  type value
  val key_gen : key Crowbar.gen
  val val_gen : value Crowbar.gen
  val pp_key : key Fmt.t
  val pp_value : value Fmt.t
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
        | Some x, Some y -> None)

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
            disjunction l r);
        Map ([map], fun m -> Map.map (fun a -> a) m);
        Map ([map], fun m -> Map.mapi (fun _ a -> a) m);
      ])

  let check_bounds map =
    match Map.min_binding map, Map.max_binding map with
    | (k1, v1) , (k2, v2) when KeyOrder.compare k1 k2 = 0 ->
      (* this only makes sense for the singleton map *)
      Crowbar.check_eq 1 @@ Map.cardinal map
    | (min, _) , (max, _) -> KeyOrder.compare max min > 0 |> Crowbar.check
    | exception Not_found -> map_eq Map.empty map

  module Equality = struct
    
    let phy_eq = Crowbar.check_eq
      ~eq:(fun x y -> x == y)
      ~cmp:(Map.compare ValueOrder.compare)
      ~pp:pp_map

    let check_add m (k, v) =
      match Map.find_opt k m with
      | Some v' when v' == v -> phy_eq m @@ Map.add k v m
      | None | Some _ -> (* any previous value will be overwritten by first
                            [add] *)
        let m_with_v = Map.add k v m in
        phy_eq m_with_v @@ Map.add k v m_with_v

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

  module Update = struct

    let val_eq = Crowbar.check_eq ~cmp:ValueOrder.compare ~pp:G.pp_value

    let nondestructive_binding map (k, v) =
      let f = function None -> Some v | e -> e in
      match Map.mem k map with
      | false -> (* inserting should always get us the element *)
        val_eq v @@ Map.find k @@ Map.update k f map
      | true -> (* inserting should always return the previous value *)
        let v' = Map.find k map in
        val_eq v' @@ Map.find k @@ Map.update k f map

    let destructive_binding map (k, v) =
      (* inserting should always get us the element *)
      val_eq v (Map.find k @@ Map.update k (fun _ -> Some v) map)

    let replace map (k, v) =
      let replace k v map =
        Map.update k (function None -> None | Some _ -> Some v) map
      in
      match Map.mem k map with
      | true ->
        val_eq v (Map.find k @@ replace k v map)
      | false ->
        Crowbar.check_eq ~pp:(Fmt.option G.pp_value) None
          (Map.find_opt k @@ replace k v map)

    (* I don't know why this is important, but it's in the unit tests, so let's
       include it *)
    let delete_extant_bind_new map (k, v) =
      let transform k v map =
        Map.update k (function None -> Some v | Some _ -> None) map
      in
      match Map.mem k map with
      | false -> (* our new binding should be there after transformation *)
        val_eq v @@ Map.find k @@ transform k v map
      | true -> 
        Crowbar.check_eq ~pp:(Fmt.option G.pp_value)
          None (Map.find_opt k @@ transform k v map)
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
module IntTester = Map_tester(OrdInt)(OrdInt)(struct
  type key = int type value = int
  let key_gen, val_gen = Crowbar.int, Crowbar.int
  let pp_key, pp_value = Fmt.int, Fmt.int
end)
module StringTester = Map_tester(String)(String)(struct
  type key = string type value = string
  let key_gen, val_gen = Crowbar.bytes, Crowbar.bytes
  let pp_key, pp_value = Fmt.string, Fmt.string
end)
module IntStringTester = Map_tester(OrdInt)(String)(struct
  type key = int type value = string
  let key_gen, val_gen = Crowbar.int, Crowbar.bytes
  let pp_key, pp_value = Fmt.int, Fmt.string
end)
(* Char is much more likely to run into key collisions *)
module CharIntTester = Map_tester(Char)(OrdInt)(struct
  type key = char type value = int
  let key_gen, val_gen = Crowbar.(Map ([uint8], Char.chr)), Crowbar.int
  let pp_key, pp_value = Fmt.char, Fmt.int
end)

let () =
  StringTester.add_tests ();
  IntTester.add_tests ();
  IntStringTester.add_tests ();
  CharIntTester.add_tests ();
