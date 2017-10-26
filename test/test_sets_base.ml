module Set_tester(Elt: Set.OrderedType) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module Set = Set.Make(Elt)

  let all_in ~f ~search ~source =
    Set.for_all (fun e -> Set.mem (f e) search) source

  let pp_set f s =
    let pp_setlist = Fmt.list G.pp in
    pp_setlist f (Set.elements s)

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
      | x, y when 0 = Elt.compare x y -> Crowbar.check_eq 1 @@ Set.cardinal s
      | x, y -> (Elt.compare x y) < 0 |> Crowbar.check
    with
    | Not_found -> Crowbar.check @@ Set.is_empty s

  let check_split_ordering s elt =
    let l, present, r = Set.split elt s in
    match Set.compare l r, present with
    | 0, false ->
      (* splitting the empty set gives 2 empty sets, which will be equal *)
      Crowbar.check @@ Set.is_empty s
    | 0, true ->
      (* if the set contains only our element, split should represent that in
         neither l nor r, so we expect equal empty sets for l and r *)
      Crowbar.check_eq ~cmp:Set.compare ~pp:pp_set s @@ Set.singleton elt
    | n, _ when n > 0 ->
      (* this should only ever happen when r is the empty set and l isn't *)
      Crowbar.check_eq ~cmp:Set.compare ~pp:pp_set r Set.empty 
    | n, _ ->
      Crowbar.check (n < 0)

  let check_split_element s elt =
    let _l, present, _r = Set.split elt s in
    Crowbar.check_eq present (Set.mem elt s)

  let check_split_echo s elt =
    let l, _present, r = Set.split elt s in
    Crowbar.check_eq false (Set.mem elt l && Set.mem elt r)

  let check_choose s =
    let s' = s in
    try
      Crowbar.check_eq ~cmp:Elt.compare ~pp:G.pp (Set.choose s) (Set.choose s')
    with
    | Not_found -> Crowbar.check (Set.is_empty s)

  let add_tests () =
    Crowbar.add_test ~name:"Set.split strictly splits on the given element"
      Crowbar.[set; G.gen] check_split_ordering;
    Crowbar.add_test ~name:"Set.split correctly reports element presence in the \
                            original set" Crowbar.[set; G.gen] check_split_element;
    Crowbar.add_test ~name:"Set.split does not include the split element in \
                            either set returned" Crowbar.[set; G.gen]
      check_split_echo;
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
    Crowbar.add_test ~name:"Set.min_elt and max_elt are equal only for sets with \
                            one element" Crowbar.[set]
      max_min_implies_singleton;
    Crowbar.add_test ~name:"Set.choose returns equal elements for equal sets"
      Crowbar.[set] check_choose;

end

module UcharTester = Set_tester(Uchar)(Shims.Uchar)
module NativeintTester = Set_tester(Nativeint)(Shims.Nativeint)
module StringTester = Set_tester(String)(Shims.String)
module IntTester = Set_tester(Shims.OrdInt)(Shims.Int)
module CharTester = Set_tester(Char)(Shims.Char)
module FloatTester = Set_tester(Shims.OrdFloat)(Shims.Float)

let () =
  UcharTester.add_tests ();
  NativeintTester.add_tests ();
  StringTester.add_tests ();
  IntTester.add_tests ();
  CharTester.add_tests ();
  FloatTester.add_tests ();
