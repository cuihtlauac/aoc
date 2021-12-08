let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

let _ =
    "day01.data"
    |> open_in
    |> Seq.unfold read_line
    |> List.of_seq
    |> List.map int_of_string
    |> (fun u -> List.hd u, List.tl u)
    |> (fun (hd, tl) -> List.fold_left (fun (i, x') x -> ((i + if x > x' then 1 else 0), x)) (0, hd) tl)
    |> fst
    |> Printf.printf "%i\n"

let _ =
    "day01.data"
    |> open_in
    |> Seq.unfold read_line
    |> List.of_seq
    |> List.map int_of_string
    |> (fun u -> List.nth u 0, List.nth u 1, List.nth u 2, u |> List.tl |> List.tl |> List.tl)
    |> (fun (u0, u1, u2, tl) -> List.fold_left (fun (x0, x1, x2, i) x3 -> (x1, x2, x3, i + if x1 + x2 + x3 > x0 + x1 + x2 then 1 else 0)) (u0, u1, u2, 0) tl)
    |> (fun (_, _, _, i) -> i)
    |> Printf.printf "%i\n"

