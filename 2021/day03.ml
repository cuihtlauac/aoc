let bin = List.fold_left (fun b c -> b * 2 + c) 0

let f path =
    let data = 
        path
        |> open_in
        |> Seq.unfold Misc.input_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun n -> int_of_char n - 48) |> List.of_seq) 
        |> List.of_seq in
    data 
    |> List.fold_left (List.map2 (+)) (List.init (List.length (List.hd data)) (Fun.const 0))
    |> List.map (fun x -> if 2 * x > List.length data then (1, 0) else (0, 1))
    |> List.split
    |> fun (u, v) -> bin u * bin v

let _ = Misc.process f 198

let weights f data =
    data
    |> List.fold_left (List.map2 (+)) (List.init (List.length (List.hd data)) (Fun.const 0))
    |> List.map (fun x -> if f (2 * x) (List.length data) then 1 else 0)

let rec iter f u =
    if List.length (fst u) = 1 then
        List.hd (fst u) 
    else 
        iter f (f u)

let f path =
    let data = 
        path
        |> open_in
        |> Seq.unfold Misc.input_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun n -> int_of_char n - 48) |> List.of_seq) 
        |> List.of_seq in
    let o = iter (fun (data, i) -> (List.filter (fun d -> List.nth d i = List.nth (weights (>=) data) i) data), i + 1) (data, 0) in
    let c = iter (fun (data, i) -> (List.filter (fun d -> List.nth d i = List.nth (weights (<) data) i) data), i + 1) (data, 0) in
    bin o * bin c

let _ = Misc.process f 230