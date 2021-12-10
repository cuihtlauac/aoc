type command = F | U | D
let to_comm = function "forward" -> F | "up" -> U | _ -> D

let f path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.map (fun str -> str |> String.split_on_char ' ' |> (fun u -> (to_comm (List.nth u 0), int_of_string (List.nth u 1))))
    |> Seq.fold_left (fun (h, d) -> (function (F, x) -> (h + x, d) | (U, x) -> (h, d - x) | (D, x) -> (h, d + x))) (0, 0)
    |> fun (f, d) -> f * d

let _ = Misc.process f 150

let f path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.map (fun str -> str |> String.split_on_char ' ' |> (fun u -> (to_comm (List.nth u 0), int_of_string (List.nth u 1))))
    |> Seq.fold_left (fun (h, d, a) -> (function (F, x) -> h + x, d + a * x, a | (U, x) -> h, d, a - x | (D, x) -> h, d, a + x)) (0, 0, 0)
    |> fun (f, d, _) -> f * d

let _ = Misc.process f 900
