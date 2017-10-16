module Set_tester(Elt: Set.OrderedType) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module Set = Set.Make(Elt)

  let set = Crowbar.(Choose [
      Map ([List G.gen], Set.of_list);
    ])

  let noddy = fun _ -> Crowbar.check true

  let add_tests () =
    Crowbar.add_test Crowbar.[set] noddy

end

module UcharTester = Set_tester(Uchar)(Shims.Uchar)

let () =
  UcharTester.add_tests ();
