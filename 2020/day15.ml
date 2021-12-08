let rec iter n f x = if n = 0 then x else iter (n - 1) f (f x)

let next (x, u) =
    let rec loop n = function
    | [] -> 0
    | y :: u -> if x = y then n else loop (n + 1) u in    
    loop 1 u, x :: u

let init u = let u = List.rev u in List.hd u, List.tl u

let _ =
    let data = [1; 12; 0; 20; 8; 16] in
    data |> init |> iter (2020 - List.length data) next |> fst |> Printf.printf "%d\n";
    (* data |> init |> iter (2_020 - List.length data) next |> fst |> Printf.printf "%d\n" *)

module IntMap = Map.Make(Int)

let next (x, (size, m)) =
    (try size - IntMap.find x m with Not_found -> 0), (size + 1, IntMap.add x size m)

let init u =
    let u = List.rev u in
    List.hd u, List.fold_right (fun n (i, m) -> i + 1, IntMap.add n i m) (List.tl u) (1, IntMap.empty)

let _ = 
    let data = [1; 12; 0; 20; 8; 16] in
    (* data |> init |> iter (2020 - List.length data) next |> fst |> Printf.printf "%d\n"; *)
    data |> init |> iter (30_000_000 - List.length data) next |> fst |> Printf.printf "%d\n"