let inv_mod m a =
    let rec euclid a b =
        if b = 0 then (a, 1, 0)
        else 
            let d, u, v = euclid b (a mod b) in
            (d, v, u - (a / b) * v) in
    let _, _, v = euclid m a in
    (v + m) mod m

let f data =
    let u = data |> List.fold_left (fun (t, u) str -> if str = "x" then (t + 1, u) else (t + 1, let y = int_of_string str in ((y - t) mod y, y) :: u)) (0, []) |> snd in
    let n = List.fold_left (fun n (_, m) -> n * m) 1 u in
    List.fold_left (fun k (a, m) -> k + let m' = n / m in a * m' * inv_mod m m') 0 u mod n

let _ = 
    let cin = "day13.input" |> open_in in
    let x = cin |> input_line |> int_of_string in
    let data = cin |> input_line |> String.split_on_char ',' in
    data |> List.filter ((<>) "x")
    |> List.fold_left (fun p s -> let n = int_of_string s in let m = n * (1 + x / n) - x in if m < snd p then (n, m) else p) (0, max_int)
    |> fun (x, y) -> Printf.printf "%d\n" (x * y);
    Printf.printf "%d\n" (f data) 
    
let _ = f ["7"; "13"; "x"; "x"; "59"; "x"; "31"; "19"]
let _ = f ["67"; "7"; "59"; "61"]
let _ = f ["67"; "x"; "7"; "59"; "61"]
