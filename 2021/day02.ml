let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

type command = F | U | D
let to_comm = function "forward" -> F | "up" -> U | _ -> D

let _ =
    "day02.data"
    |> open_in
    |> Seq.unfold read_line
    |> Seq.map (fun str -> str |> String.split_on_char ' ' |> (fun u -> (to_comm (List.nth u 0), int_of_string (List.nth u 1))))
    |> Seq.fold_left (fun (h, d) -> (function (F, x) -> (h + x, d) | (U, x) -> (h, d - x) | (D, x) -> (h, d + x))) (0, 0)
    |> (fun (f, d) -> Printf.printf "%i\n" (f * d))

let _ =
    "day02.data"
    |> open_in
    |> Seq.unfold read_line
    |> Seq.map (fun str -> str |> String.split_on_char ' ' |> (fun u -> (to_comm (List.nth u 0), int_of_string (List.nth u 1))))
    |> Seq.fold_left (fun (h, d, a) -> (function (F, x) -> (h + x, d + a * x, a) | (U, x) -> (h, d, a - x) | (D, x) -> (h, d, a + x))) (0, 0, 0)
    |> (fun (f, d, _) -> Printf.printf "%i\n" (f * d))