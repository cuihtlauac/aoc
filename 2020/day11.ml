type seat = Floor | Empty | Occupied

let seat_of_char = function '.' -> Floor | 'L' -> Empty | _ -> Occupied

let array_of_line str = str |> String.to_seq |> Seq.map seat_of_char |> Array.of_seq
let read_line cin = try Some (cin |> Stdlib.input_line |> array_of_line, cin) with End_of_file -> None
let read_data str = str |> open_in |> Seq.unfold read_line |> Array.of_seq 
let copy src = Array.init (Array.length src) (fun i -> Array.copy src.(i))

let touch data (di, dj) (i, j) = try if data.(i + di).(j + dj) = Occupied then 1 else 0 with _ -> 0
let rec sight data (di, dj) (i, j) =
    let (i, j) = (i + di, j + dj) in
    try
        match data.(i).(j) with
        | Floor -> sight data (di, dj) (i, j)
        | Empty -> 0
        | Occupied -> 1
    with _ -> 0 

let neighbourhood get k src dst (i, j) = function
| Floor -> ()
| x ->
    let bearing = [(+1, -1); (+1, 0); (+1, +1); (0, -1); (0, +1); (-1, -1); (-1, 0); (-1, +1)] in
    let n = List.fold_left (fun n v -> n + get src v (i, j)) 0 bearing in
    dst.(i).(j) <- if x = Empty && n = 0 then Occupied
    else if x = Occupied && n >= k then Empty 
    else x

let rec fix f src dst =
    Array.iteri (fun i -> Array.iteri (fun j -> f src dst (i, j))) src;
    if src = dst then
        Array.fold_left (Array.fold_left (fun n seat -> n + if seat = Occupied then 1 else 0)) 0 src
    else 
        fix f dst src

let _ =
    let data = "day11.input" |> read_data in
    fix (neighbourhood touch 4) (copy data) (copy data) |> Printf.printf "%d\n";
    fix (neighbourhood sight 5) (copy data) (copy data) |> Printf.printf "%d\n"
