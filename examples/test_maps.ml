module IntMap = Map.Make(struct type t = int let compare x y = x-y end)

let show m = IntMap.iter (fun k v -> Printf.printf "%d -> %d\n" k v) m

let update x f m =
  let yp = IntMap.find_opt x m in
  let y = f yp in
  match yp, y with
  | _, None -> IntMap.remove x m
  | None, Some z -> IntMap.add x z m
  | Some zp, Some z -> if zp == z then m else IntMap.add x z m

let map_gen n =
  let rec init m = function
    | -1 -> m
    | n -> init (IntMap.add n n m) (n - 1)
  in
  init IntMap.empty n

let check n =
  let map = map_gen n in
  Crowbar.check ((=) 0 @@ compare (IntMap.cardinal map) n)

let () =
  Crowbar.add_test ~name:"init" Crowbar.[int] @@ fun n -> check n
