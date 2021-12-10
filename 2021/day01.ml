
let f1 path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> (|>) ()
    |> function Seq.Nil -> failwith "" | Seq.Cons (str, seq) -> let x = int_of_string str in seq 
    |> Seq.fold_left (fun (i, x) str -> let y = int_of_string str in i + max 0 (compare y x), y) (0, x)
    |> fst 

let _ = Misc.process f1 7

let f2 path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> (|>) ()
    |> function Seq.Nil -> failwith "" | Seq.Cons (x0, seq) -> let x0 = int_of_string x0 in seq ()
    |> function Seq.Nil -> failwith "" | Seq.Cons (x1, seq) -> let x1 = int_of_string x1 in seq ()
    |> function Seq.Nil -> failwith "" | Seq.Cons (x2, seq) -> let x2 = int_of_string x2 in seq 
    |> Seq.fold_left (fun (x0, x1, x2, i) x3 -> let x3 = int_of_string x3 in (x1, x2, x3, i + if x3 > x0 then 1 else 0)) (x0, x1, x2, 0)
    |> fun (_, _, _, i) -> i

    let _ = Misc.process f2 5
  