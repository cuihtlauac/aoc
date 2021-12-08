let read_line cin = try Some (Scanf.bscanf cin "%d\n" Fun.id, cin) with End_of_file -> None
let read_data str = str |> Scanf.Scanning.open_in |> Seq.unfold read_line |> List.of_seq |> List.sort (Fun.flip compare) 

let data = "day10.input" |> read_data

let f (prev, diff1, diff3) x =
    match x - prev with
    | 1 -> (x, diff1 + 1, diff3)
    | 3 -> (x, diff1, diff3 + 1)
    | _ -> (x, diff1, diff3)

(*
let _ = let _, x, y = data1 |> fun u -> List.hd u + 3 :: u |> List.rev |> List.fold_left f (0, 0, 0) in Printf.printf "%d*%d=%d\n" x y (x * y)
let _ = let _, x, y = data2 |> fun u -> List.hd u + 3 :: u |> List.rev |> List.fold_left f (0, 0, 0) in Printf.printf "%d*%d=%d\n" x y (x * y)
*)
let _ = let _, x, y = data |> fun u -> List.hd u + 3 :: u |> List.rev |> List.fold_left f (0, 0, 0) in Printf.printf "%d\n" (x * y)

let rec para f = function [] -> f None | a :: u -> f (Some (a, (u, para f u)))

let rec take n = function
| x :: u -> if n > 0 then x :: take (n - 1) u else []
| u -> u

let g = function 
| None -> []
| Some (a, (u, b)) ->
    let n = u |> take 3 |> List.filter (fun x -> a - x <= 3) |> List.length in
    let s = if n = 0 then 1 else b |> take n |> List.fold_left (+) 0 in
    s :: b
(*
let _ = data1 |> List.rev |> List.cons 0 |> List.rev |> para g |> List.hd |> Printf.printf "%d\n"
let _ = data2 |> List.rev |> List.cons 0 |> List.rev |> para g |> List.hd |> Printf.printf "%d\n"
*)
let _ = data |> List.rev |> List.cons 0 |> List.rev |> para g |> List.hd |> Printf.printf "%d\n"
