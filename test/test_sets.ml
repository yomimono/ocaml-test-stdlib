module Set_tester(Elt: Set.OrderedType) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module Set = Set.Make(Elt)

  let all_in ~f ~search ~source =
    Set.for_all (fun e -> Set.mem (f e) search) source

  let rec set : Set.t Crowbar.gen = Crowbar.(Choose [
      Const Set.empty;
      Map ([List G.gen], Set.of_list);
      Map ([G.gen; set], Set.add);
      Map ([G.gen; set], Set.remove);
      Map ([G.gen], Set.singleton);
      Map ([set; set], Set.union);
      Map ([set; set], Set.inter);
      Map ([set; set], Set.diff);
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

  let check_map s =
    let s' = Set.map G.transform s in
    all_in ~source:s ~search:s' ~f:G.transform |> Crowbar.check

  let check_union s1 s2 =
    let u = Set.union s1 s2 in
    ((all_in ~f:(fun a -> a) ~source:s1 ~search:u) &&
     (all_in ~f:(fun a -> a) ~source:s2 ~search:u)) |> Crowbar.check

  let check_intersection s1 s2 =
    let i = Set.inter s1 s2 in
    (* everything in i should be in both s1 and s2 *)
    ((all_in ~f:(fun a -> a) ~source:i ~search:s1) &&
    (all_in ~f:(fun a -> a) ~source:i ~search:s2) &&
    (* everything that is in s1 and is also in s2 should be in i *)
    (all_in ~f:(fun a -> a) ~source:(Set.filter (fun e -> Set.mem e s2) s1)
       ~search:i) &&
    (* everything that is in s2 and is also in s1 should be in i *)
    (all_in ~f:(fun a -> a) ~source:(Set.filter (fun e -> Set.mem e s1) s2)
       ~search:i)) |> Crowbar.check

  let check_diff s1 s2 =
    let d = Set.diff s1 s2 in
    (* all items in s1, if not in s2, should be present in d *)
    ((all_in ~f:(fun a -> a) ~source:(Set.filter (fun e -> not (Set.mem e s2)) s1)
       ~search:d) &&
    (* no items in both s1 and s2 should be in d *)
    Set.(is_empty (inter d @@ inter s1 s2))) |> Crowbar.check

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
    Crowbar.add_test ~name:"Set.map represents f(x) for all items in the input \
                            set" Crowbar.[set] check_map;
    Crowbar.add_test ~name:"Set.union contains each element in both sets"
      Crowbar.[set; set] check_union;
    Crowbar.add_test ~name:"Set.inter contains every element in both \
                            sets" Crowbar.[set; set] check_intersection;
    Crowbar.add_test ~name:"Set.diff contains only elements in first set not \
                            present in the second set" Crowbar.[set; set]
      check_diff;

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
