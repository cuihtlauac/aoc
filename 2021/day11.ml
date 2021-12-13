
let rec iter f n x = if n = 0 then x else iter f (n - 1) (f x)

let rec fix f x = let y = f x in if x = y then x else fix f y

let rec reach f y x = if x = y then 0 else 1 + reach f y (f x) 

let flash mat i j cell =
    if 0 < cell && cell <= 9 then
        [(i + 1, j + 1); (i + 1, j); (i + 1, j - 1);(i, j + 1); (i, j - 1);(i - 1, j + 1); (i - 1, j); (i - 1, j - 1)]
        |> List.fold_left (fun n (i, j) -> n + try max (compare mat.(i).(j) 9) 0 with _ -> 0) cell
    else 0

let bigstep (n, mat) = 
    mat
    |> Array.map (Array.map (fun cell -> cell + 1))
    |> fix (fun mat -> Array.mapi (fun i -> Array.mapi (flash mat i)) mat)
    |> fun mat -> (Array.fold_left (Array.fold_left (fun n cell -> n + if cell > 0 then 0 else 1)) n mat, mat)

let f path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
    |> Array.of_seq
    |> (fun mat -> iter bigstep 100 (0, mat))
    |> fst

let _ = Misc.process f 1656

let bigstep mat = 
    mat
    |> Array.map (Array.map (fun cell -> cell + 1))
    |> fix (fun mat -> Array.mapi (fun i -> Array.mapi (flash mat i)) mat)

let f path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
    |> Array.of_seq
    |> reach bigstep (Array.make_matrix 10 10 0)

let _ = Misc.process f 195