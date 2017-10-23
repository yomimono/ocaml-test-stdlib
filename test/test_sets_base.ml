module Set_tester(Elt: Set.OrderedType) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module Set = Set.Make(Elt)

  let all_in ~f ~search ~source =
    Set.for_all (fun e -> Set.mem (f e) search) source

  let rec set : Set.t Crowbar.gen = Crowbar.(Choose [
      Const Set.empty;
      Map ([List1 G.gen], Set.of_list);
      Map ([G.gen; set], Set.add);
      Map ([G.gen; set], Set.remove);
      Map ([G.gen], Set.singleton);
      Map ([set; set], Set.union);
      Map ([set; set], Set.inter);
      Map ([set; set], Set.diff);
      Map ([set], Set.map G.transform);
      Map ([set], Set.filter (fun _ -> true));
      Map ([set], fun s -> Set.partition (fun _ -> true) s |> fst);
      Map ([G.gen; set], (fun e s ->
          let l, _, r = Set.split e s in
          Set.union l r));
    ])

  let check_min_max s =
    match Set.(min_elt s, max_elt s) with
    | min, max when Elt.compare min max = 0 ->
      Crowbar.check_eq 1 @@ Set.cardinal s
    | min, max -> Elt.compare max min > 0 |> Crowbar.check
    | exception Not_found -> Crowbar.check @@ Set.is_empty s

  let check_add_cardinality s elt =
    match Set.mem elt s with
    | true -> Crowbar.check_eq Set.(cardinal s) Set.(add elt s |> cardinal)
    | false -> Crowbar.check_eq
                 ((Set.cardinal s) + 1) Set.(add elt s |> cardinal)

  let check_add_equality s elt =
    match Set.mem elt s with
    | true -> Crowbar.check_eq ~eq:(fun x y -> x == y) s (Set.add elt s)
    | false ->
      let s = Set.add elt s in
      Crowbar.check_eq ~eq:(==) s (Set.add elt s)

  let check_remove_cardinality s elt =
    match Set.mem elt s with
    | true -> Crowbar.check_eq ((Set.cardinal s) - 1) Set.(remove elt s |> cardinal)
    | false -> Crowbar.check_eq Set.(cardinal s) Set.(remove elt s |> cardinal)

  let check_remove_equality s elt =
    match Set.mem elt s with
    | false -> Crowbar.check_eq ~eq:(fun x y -> x == y) s (Set.remove elt s)
    | true ->
      let s = Set.remove elt s in
      Crowbar.check_eq ~eq:(==) s (Set.remove elt s)

  let check_map_equality s =
    Crowbar.check_eq ~eq:(==) s (Set.map (fun a -> a) s)

  let check_filter_equality s =
    Crowbar.check_eq ~eq:(==) s (Set.filter (fun _ -> true) s)

  let check_map s =
    let s' = Set.map G.transform s in
    all_in ~source:s ~search:s' ~f:G.transform |> Crowbar.check

  let max_min_implies_singleton s =
    try
      match Set.min_elt s, Set.max_elt s with
      | x, y when x = y -> Crowbar.check_eq 1 @@ Set.cardinal s
      | x, y -> (Elt.compare x y) < 0 |> Crowbar.check
    with
    | Not_found -> Crowbar.check @@ Set.is_empty s

  let add_tests () =
    Crowbar.add_test ~name:"Set.min >= Set.max only when the set has 1 element"
      Crowbar.[set] check_min_max;
    Crowbar.add_test ~name:"Set.add never results in a set with fewer elements"
      Crowbar.[set; G.gen] check_add_cardinality;
    Crowbar.add_test ~name:"Set.add of an element present preserves physical \
                            equality" Crowbar.[set; G.gen] check_add_equality;
    Crowbar.add_test ~name:"Set.remove on set without that element preserves \
                            physical equality" Crowbar.[set; G.gen]
      check_remove_equality;
    Crowbar.add_test ~name:"Set.map with identity function preserves physical \
                            equality" Crowbar.[set] check_map_equality;
    Crowbar.add_test ~name:"Set.filter with identity function preserves physical \
                            equality" Crowbar.[set] check_filter_equality;
    Crowbar.add_test ~name:"Set.map represents f(x) for all items in the input \
                            set" Crowbar.[set] check_map;
    Crowbar.add_test ~name:"min_elt and max_elt are equal only for sets with \
                            one element" Crowbar.[set]
      max_min_implies_singleton;

end

module UcharTester = Set_tester(Uchar)(Shims.Uchar)
module NativeintTester = Set_tester(Nativeint)(Shims.Nativeint)
module StringTester = Set_tester(String)(Shims.String)
module IntTester = Set_tester(Shims.OrdInt)(Shims.Int)
module CharTester = Set_tester(Char)(Shims.Char)

let () =
  UcharTester.add_tests ();
  NativeintTester.add_tests ();
  StringTester.add_tests ();
  IntTester.add_tests ();
  CharTester.add_tests ();
