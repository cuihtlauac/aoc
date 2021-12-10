let rec iter f n x = if n = 0 then x else iter f (n - 1) (f x) 

let rec loop = function
| x :: u -> let xs, u = Misc.span ((=) x) [] u in (int_of_string x, List.length (x :: xs)) :: loop u
| [] -> []

let add_fish (f0, f1, f2, f3, f4, f5, f6, f7, f8) = function
| (0, n) -> f0 + n, f1, f2, f3, f4, f5, f6, f7, f8
| (1, n) -> f0, f1 + n, f2, f3, f4, f5, f6, f7, f8
| (2, n) -> f0, f1, f2 + n, f3, f4, f5, f6, f7, f8
| (3, n) -> f0, f1, f2, f3 + n, f4, f5, f6, f7, f8
| (4, n) -> f0, f1, f2, f3, f4 + n, f5, f6, f7, f8
| (5, n) -> f0, f1, f2, f3, f4, f5 + n, f6, f7, f8
| (6, n) -> f0, f1, f2, f3, f4, f5, f6 + n, f7, f8
| (7, n) -> f0, f1, f2, f3, f4, f5, f6, f7 + n, f8
| (_, n) -> f0, f1, f2, f3, f4, f5, f6, f7, f8 + n

let f n path =
  path
  |> open_in
  |> Stdlib.input_line
  |> String.split_on_char ','
  |> List.sort compare
  |> loop
  |> List.fold_left add_fish (0, 0, 0, 0, 0, 0, 0, 0, 0)
  |> iter (fun (f0, f1, f2, f3, f4, f5, f6, f7, f8) -> f1, f2, f3, f4, f5, f6, f0 + f7, f8, f0) n
  |> (fun (f0, f1, f2, f3, f4, f5, f6, f7, f8) -> f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8)

  let _ = Misc.process (f 80) 5934
  let _ = Misc.process (f 256) 26984457539