let rec seq_hylo f g z x = match f x with Some (y, x) -> seq_hylo f g (g z y) x | None -> z

let read_line cin = try Some (Scanf.bscanf cin "%c%i\n" (fun x y -> (x, y)), cin) with End_of_file -> None

let move (x, y, h) = function
| ('N', d) -> (x, y + d, h)
| ('S', d) -> (x, y - d, h)
| ('E', d) -> (x + d, y, h)
| ('W', d) -> (x - d, y, h)
| ('L', d) -> (x, y, (h + d / 90) mod 4)
| ('R', d) -> (x, y, (h + 4 - d / 90) mod 4)
| (_, d) when h = 0 -> (x, y + d, h) (* N *)
| (_, d) when h = 1 -> (x - d, y, h) (* W *)
| (_, d) when h = 2 -> (x, y - d, h) (* S *)
| (_, d) -> (x + d, y, h) (* E *)

let _ =
"day12.input" |> Scanf.Scanning.open_in 
|> seq_hylo read_line move (0, 0, 3)
|> fun (x, y, _) -> Printf.printf "%d\n" (abs x + abs y)

let move' (x, y, wx, wy) = function
| ('N', d) -> (x, y, wx, wy + d)
| ('S', d) -> (x, y, wx, wy - d)
| ('E', d) -> (x, y, wx + d, wy)
| ('W', d) -> (x, y, wx - d, wy)
| ('F', d) -> (x + d * wx, y + d * wy, wx, wy)
| ('L', d) when d = 90 -> (x, y, -wy, wx)
| ('R', d) when d = 270 -> (x, y, -wy, wx)
| ('L', d) when d = 270 -> (x, y, wy, -wx)
| ('R', d) when d = 90 -> (x, y, wy, -wx)
| _ -> (x, y, -wx, -wy) (* U-turn *)

let _ =
"day12.input" |> Scanf.Scanning.open_in
|> seq_hylo read_line move' (0, 0, 10, 1)
|> fun (x, y, _, _) -> Printf.printf "%d\n" (abs x + abs y)


