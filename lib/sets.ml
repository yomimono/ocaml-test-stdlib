open Base

module Set_tester(Elt: Shims.Comparably_buildable) (G: Shims.GENERABLE with type t = Elt.t)
= struct
  module EltComparison = Comparable.Make_using_comparator(Elt)

  let all_in ~fn ~search ~source =
    Set.for_all ~f:(fun e -> Set.mem search (fn e)) source

  let sexp_of_set = Set.sexp_of_m__t (module Elt)

  let pp_set f s = Sexp.pp f (sexp_of_set s)

  let rec set = lazy (Crowbar.(choose [
      const @@ Set.empty (module Elt);
      map [list1 G.gen] @@ Set.of_list (module Elt);
      map [(unlazy set); G.gen] Set.add;
      map [(unlazy set); G.gen] Set.remove;
      map [G.gen] @@ Set.singleton (module Elt);
      map [(unlazy set); (unlazy set)] Set.union;
      map [(unlazy set); (unlazy set)] Set.inter;
      map [(unlazy set); (unlazy set)] Set.diff;
      map [(unlazy set)] (fun s -> Set.map (module Elt) s ~f:G.transform);
      map [(unlazy set)] (fun s -> Set.filter s ~f:(fun _ -> true));
      map [(unlazy set)] (fun s -> Set.partition_tf s ~f:(fun _ -> true) |> fst);
      map [G.gen; (unlazy set)] ((fun e s ->
          let l, _, r = Set.split s e  in
          Set.union l r));
    ]))

  let lazy set = set

  let check_min_max s =
    match Set.(min_elt s, max_elt s) with
    | Some min, Some max when EltComparison.compare min max = 0 ->
      Crowbar.check_eq 1 @@ Set.length s
    | Some min, Some max -> EltComparison.compare max min > 0 |> Crowbar.check
    | None, None -> Crowbar.check @@ Set.is_empty s
    | Some _, None | None, Some _ -> Crowbar.fail "Set.min_elt and Set.max_elt disagree on whether the set is empty"

  let check_add_cardinality s elt =
    match Set.mem s elt with
    | true -> Crowbar.check_eq Set.(length s) Set.(add s elt |> length)
    | false -> Crowbar.check_eq
                 ((Set.length s) + 1) Set.(add s elt |> length)

  let check_remove_cardinality s elt =
    match Set.mem elt s with
    | true -> Crowbar.check_eq ((Set.length s) - 1) Set.(remove elt s |> length)
    | false -> Crowbar.check_eq Set.(length s) Set.(remove elt s |> length)

  let check_map s =
    let s' = Set.map (module Elt) ~f:G.transform s in
    all_in ~source:s ~search:s' ~fn:G.transform |> Crowbar.check

  let check_split_ordering s elt =
    let l, present, r = Set.split s elt in
    match Set.compare_direct l r, present with
    | 0, None ->
      (* splitting the empty set gives 2 empty sets, which will be equal *)
      Crowbar.check @@ Set.is_empty s
    | 0, Some _ ->
      (* if the set contains only our element, split should represent that in
         neither l nor r, so we expect equal empty sets for l and r *)
      Crowbar.check_eq ~cmp:Set.compare_direct ~pp:pp_set s @@ Set.singleton (module Elt) elt
    | n, _ when n > 0 ->
      (* this should only ever happen when r is the empty set and l isn't *)
      Crowbar.check_eq ~cmp:Set.compare_direct ~pp:pp_set r @@ Set.empty (module Elt)
    | n, _ ->
      Crowbar.check (n < 0)

  let check_split_element s elt =
    let _l, present, _r = Set.split s elt in
    match present with
    | Some e -> Crowbar.check (Set.mem s e)
    | None -> Crowbar.check (not @@ Set.mem s elt)

  (* "echo"? *)
  let check_split_echo s elt =
    let l, _present, r = Set.split s elt in
    Crowbar.check_eq false (Set.mem l elt && Set.mem r elt)

(* there is a choose, but no promises made about an equal set returning the same
   element when choose is called. *)
  let check_choose s =
    match Set.choose s with
    | None -> Crowbar.check (Set.is_empty s)
    | Some _ -> Crowbar.check (0 < Set.length s)

  (* base gives us `invariants`, so use it! *)
  let invariants_hold s = Crowbar.check (Set.invariants s)

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
    Crowbar.add_test ~name:"Set.map represents f(x) for all items in the input \
                            set" Crowbar.[set] check_map;
    Crowbar.add_test ~name:"Set.choose returns consistent results for \
                            empty/nonempty sets" Crowbar.[set] check_choose;
    Crowbar.add_test ~name:"Set.invariants is true" Crowbar.[set] invariants_hold;

end

module UcharTester = Set_tester(Uchar)(Shims.Uchar)
module NativeintTester = Set_tester(Nativeint)(Shims.Nativeint)
module StringTester = Set_tester(String)(Shims.String)
module IntTester = Set_tester(Int)(Shims.Int)
module CharTester = Set_tester(Char)(Shims.Char)
module FloatTester = Set_tester(Float)(Shims.Float)
