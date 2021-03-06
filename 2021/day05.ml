let line x1 y1 x2 y2 =
    let min, max = min (x1, y1) (x2, y2), max (x1, y1) (x2, y2) in
    let delta_x = compare (fst max) (fst min) in
    let delta_y = compare (snd max) (snd min) in
    Seq.unfold (fun xy -> if max >= xy then Some (xy, (fst xy + delta_x, snd xy + delta_y)) else None) min

let rec loop = function
| x :: u -> let xs, u = Misc.span ((=) x) [] u in loop u + compare (List.length xs) 0
| [] -> 0

let f path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.concat_map (fun str -> Scanf.sscanf str "%i,%i -> %i,%i" (fun x1 y1 x2 y2 -> if x1 = x2 || y1 = y2 then line x1 y1 x2 y2 else Seq.empty))
    |> List.of_seq
    |> List.sort compare
    |> loop

let _ = Misc.process f 5

let f path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.concat_map (fun str -> Scanf.sscanf str "%i,%i -> %i,%i" line)
    |> List.of_seq
    |> List.sort compare
    |> loop

let _ = Misc.process f 12
