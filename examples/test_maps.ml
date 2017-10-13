module Map = Map.Make (struct
  type t = int
  let compare (i : int) (j : int) = compare i j
end)

let int_pair = Crowbar.(Map ([int; int], fun x y -> x, y))

let map = Crowbar.(Choose [
    Const Map.empty;
    Map ([int_pair], fun (x, y) -> Map.singleton x y);
    Map ([List int_pair], fun items ->
        List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty items);
  ])

let check_bounds map =
  Crowbar.check @@ try
    match Map.min_binding map, Map.max_binding map with
    | (min, _) , (max, _) when compare max min = 1 -> true
    | (min, _) , (max, _) when compare max min = -1 -> false
    | (min, _) , (max, _) ->
      Map.for_all (fun k _ -> compare k min = 0) map
  with
  | Not_found -> 0 = Map.cardinal map

let nondestructive_binding map (k, v) =
  Crowbar.check @@
  try
    match Map.mem k map with
    | false -> (* inserting should always get us the element *)
      0 = v - (Map.find k @@ Map.update k (function None -> Some v | e -> e) map)
    | true -> (* inserting should always return the previous value *)
      let v' = Map.find k map in
      0 = v' - (Map.find k @@ Map.update k (function None -> Some v | e -> e) map)
  with
  | Not_found -> false

let destructive_binding map (k, v) =
  Crowbar.check @@
  try
    (* inserting should always get us the element *)
    0 = v - (Map.find k @@ Map.update k (fun _ -> Some v) map)
  with
  | Not_found -> false

let () =
  (* Crowbar.add_test ~name:"max_binding = min_binding implies all elements are equal"
    Crowbar.[map] check_bounds;
  Crowbar.add_test ~name:"non-destructive updates never shadow existing bindings"
    Crowbar.[map; int_pair] nondestructive_binding; *)
  Crowbar.add_test ~name:"destructive updates always shadow existing bindings"
    Crowbar.[map; int_pair] destructive_binding;
