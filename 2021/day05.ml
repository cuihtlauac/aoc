let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

let rec span f acc = function
| x :: u when f x -> span f (x :: acc) u
| u -> acc, u 

let line x1 y1 x2 y2 =
    let min, max = min (x1, y1) (x2, y2), max (x1, y1) (x2, y2) in
    let delta_x = compare (fst max) (fst min) in
    let delta_y = compare (snd max) (snd min) in
    Seq.unfold (fun xy -> if max >= xy then Some (xy, (fst xy + delta_x, snd xy + delta_y)) else None) min

let rec loop = function
| x :: u -> let xs, u = span ((=) x) [] u in loop u + compare (List.length xs) 0 
| [] -> 0

let _ =
    "day05.data"
    |> open_in
    |> Seq.unfold read_line
    |> Seq.concat_map (fun str -> Scanf.sscanf str "%i,%i -> %i,%i" (fun x1 y1 x2 y2 -> if x1 = x2 || y1 = y2 then line x1 y1 x2 y2 else Seq.empty))
    |> List.of_seq
    |> List.sort compare
    |> loop
    |> Printf.printf "%i\n"

let _ =
    "day05.data"
    |> open_in
    |> Seq.unfold read_line
    |> Seq.concat_map (fun str -> Scanf.sscanf str "%i,%i -> %i,%i" line)
    |> List.of_seq
    |> List.sort compare
    |> loop
    |> Printf.printf "%i\n"
    