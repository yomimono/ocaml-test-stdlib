module Set_tester(Elt: Set.OrderedType) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module Set = Set.Make(Elt)

  let set = Crowbar.(Choose [
      Map ([List G.gen], Set.of_list);
    ])

  let check_min_max s =
    match Set.(min_elt s, max_elt s) with
    | min, max when Elt.compare min max = 0 ->
      Crowbar.check_eq 1 @@ Set.cardinal s
    | min, max -> Elt.compare max min > 0 |> Crowbar.check
    | exception Not_found -> Crowbar.check @@ Set.is_empty s


  let add_tests () =
    Crowbar.add_test ~name: "Set.min >= Set.max only when the set has 1 element"
      Crowbar.[set] check_min_max

end

module UcharTester = Set_tester(Uchar)(Shims.Uchar)

let () =
  UcharTester.add_tests ();
