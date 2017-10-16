module Set_tester(Elt: Set.OrderedType) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module Set = Set.Make(Elt)

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
    Set.((cardinal s) <= (add elt s |> cardinal)) |> Crowbar.check

  let check_add_equality s elt =
    match Set.mem elt s with
    | true -> Crowbar.check_eq ~eq:(fun x y -> x == y) s (Set.add elt s)
    | false ->
      let s = Set.add elt s in
      Crowbar.check_eq ~eq:(fun x y -> x == y) s (Set.add elt s)

  let add_tests () =
    Crowbar.add_test ~name: "Set.min >= Set.max only when the set has 1 element"
      Crowbar.[set] check_min_max;
    Crowbar.add_test ~name: "Set.add never results in a set with fewer elements"
      Crowbar.[set; G.gen] check_add_cardinality;

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
