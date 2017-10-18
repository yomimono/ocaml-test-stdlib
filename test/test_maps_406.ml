module Map_tester(KeyOrder: Map.OrderedType)(ValueOrder: Map.OrderedType)
    (G: Test_maps_base.GENERATOR with type key = KeyOrder.t and type value = ValueOrder.t) = struct
  include Test_maps_base.Map_tester(KeyOrder)(ValueOrder)(G)
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
  let add_tests () =
    Update.(
      Crowbar.add_test ~name:"destructive updates always shadow existing bindings"
        Crowbar.[map; pair] destructive_binding;
      Crowbar.add_test ~name:"non-destructive updates never shadow existing bindings"
        Crowbar.[map; pair] nondestructive_binding;
      Crowbar.add_test ~name:"replacing does not create new bindings"
        Crowbar.[map; pair] replace;
      Crowbar.add_test ~name:"delete-if-present transformation works"
        Crowbar.[map; pair] delete_extant_bind_new;
    )
end
module StringString = Test_maps_base.Make_generator(Shims.String)(Shims.String)
module IntString = Test_maps_base.Make_generator(Shims.Int)(Shims.String)
module CharInt = Test_maps_base.Make_generator(Shims.Char)(Shims.Int)
module NativeintInt = Test_maps_base.Make_generator(Shims.Nativeint)(Shims.Int)
module UcharString = Test_maps_base.Make_generator(Shims.Uchar)(Shims.String)

module StringTester = Map_tester(String)(String)(StringString)
module IntStringTester = Map_tester(Shims.OrdInt)(String)(IntString)
module CharIntTester = Map_tester(Char)(Shims.OrdInt)(CharInt)
module NativeIntTester = Map_tester(Nativeint)(Shims.OrdInt)(NativeintInt)
module UcharStringTester = Map_tester(Uchar)(String)(UcharString)

let () =
  StringTester.add_tests ();
  IntStringTester.add_tests ();
  CharIntTester.add_tests ();
  NativeIntTester.add_tests ();
  UcharStringTester.add_tests ();
