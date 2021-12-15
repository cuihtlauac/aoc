let (>>=) u f = List.concat_map f u

let rec list_unfold f x = match f x with Some (y, x') -> y :: list_unfold f x' | _ -> []

let diags len =
    let a = List.init len (fun j -> (0, j)) >>= list_unfold (fun (i, j) -> if j >= 0 then Some ((i, j), (i + 1, j - 1)) else None) in
    let b = List.init (len - 1) (fun i -> (i + 1, len - 1)) >>= list_unfold (fun (i, j) -> if i < len then Some ((i, j), (i + 1, j - 1)) else None) in
    a @ b

let def f x y z = if x = z then y else f z

let f path =
    let mat = 
        path
        |> open_in
        |> Seq.unfold Misc.input_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
        |> Array.of_seq in
    let len = Array.length mat in
    diags len
    |> List.fold_left (fun f (i, j) -> def f (i, j) (mat.(i).(j) + min (f (i - 1, j)) (f (i, j - 1)))) (fun (i, j) -> if i + j < 0 then 0 else max_int) 
    |> (|>) (len - 1, len -1)
    |> fun n -> n - mat.(0).(0)

let _ = Misc.process f 40
