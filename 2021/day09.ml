let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

let neighbour data i j = [i + 1, j; i - 1, j; i, j + 1; i, j - 1] |> List.filter_map (fun (i, j) -> try Some (i, j, data.(i).(j)) with Invalid_argument _ -> None)

let low_height data i j = 
    let neighbour = neighbour data i j |> List.map (fun (_, _, x) -> x) in
    if List.fold_left (fun b n -> b && data.(i).(j) < n) true neighbour then 1 + data.(i).(j) else 0

let _ =
    let data = 
    "day09.data"
        |> open_in
        |> Seq.unfold read_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
        |> Array.of_seq in
    data
    |> Array.fold_left (fun (s, i) row -> ((row |> Array.fold_left (fun (s, j) cell -> (s + low_height data i j, j + 1)) (s, 0) |> fst), i + 1)) (0, 0)
    |> fst
    |> Printf.printf "%i\n"

let low data i j = 
    let neighbour = neighbour data i j |> List.map (fun (_, _, x) -> x) in
    if List.fold_left (fun b n -> b && data.(i).(j) < n) true neighbour then [i, j, data.(i).(j)] else []

module PointSet = Set.Make(struct type t = int * int * int let compare = compare end)

let rec bassin data acc (i, j, x) =
    if PointSet.mem (i, j, x) acc then acc else
        let acc = PointSet.add (i, j, x) acc in
        neighbour data i j
        |> List.filter (fun (i', j', x') -> x < x' && x' < 9 && not (PointSet.mem (i', j', x') acc))
        |> List.fold_left (fun acc (i', j', x') -> bassin data acc (i', j', x')) acc

let _ =
    let data = 
    "day09.data"
        |> open_in
        |> Seq.unfold read_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
        |> Array.of_seq in
    data
    |> Array.fold_left (fun (ll, i) row -> ((row |> Array.fold_left (fun (ll, j) cell -> (low data i j @ ll, j + 1)) (ll, 0) |> fst), i + 1)) ([], 0)
    |> fst
    |> List.map (fun p -> p |> bassin data PointSet.empty |> PointSet.cardinal)
    |> List.sort (Fun.flip compare)
    |> fun u -> Printf.printf "%i\n" (List.nth u 0 * List.nth u 1 * List.nth u 2) 
    
    
