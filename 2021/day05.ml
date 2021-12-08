let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

let rec list_unfold f x : 'a list = match f x with
| None -> [] 
| Some (y, x) -> y :: list_unfold f x

let hori x (y_min, y_max) = list_unfold (fun y -> if y <= y_max then Some ((x, y), y + 1) else None) y_min
let vert y (x_min, x_max) = list_unfold (fun x -> if x <= x_max then Some ((x, y), x + 1) else None) x_min

let mono a b = min a b, max a b

let f ((x1, y1), (x2, y2)) = if x1 = x2 || y1 = y2 then Some ((x1, y1), (x2, y2)) else None

let rec grow acc a = function
| x :: u when x = a -> grow (x :: acc) a u
| u -> acc, u 

let rec loop = function
| x :: u -> let xs, u = grow [] x u in (x :: xs) :: loop u
| [] -> []

let line (x1, y1) (x2, y2) =
    let min = min (x1, y1) (x2, y2) in
    let max = max (x1, y1) (x2, y2) in
    let delta_x = compare (fst max) (fst min) in
    let delta_y = compare (snd max) (snd min) in
    list_unfold (fun (x, y) -> if (x, y) <= max then Some ((x, y), (x + delta_x, y + delta_y)) else None) min

let a =
    "day05.data"
    |> open_in
    |> Seq.unfold read_line
    |> List.of_seq
    |> List.filter_map (fun str -> Scanf.sscanf str "%i,%i -> %i,%i" (fun x1 y1 x2 y2 -> if x1 = x2 then Some (hori x1 (mono y1 y2)) else if y1 = y2 then Some (vert y1 (mono x1 x2)) else None))
    |> List.concat
    |> List.sort compare
    |> loop
    |> List.filter (fun u -> List.length u > 1)
    |> List.length
    |> Printf.printf "%i\n"

let a =
    "day05.data"
    |> open_in
    |> Seq.unfold read_line
    |> List.of_seq
    |> List.map (fun str -> Scanf.sscanf str "%i,%i -> %i,%i" (fun x1 y1 x2 y2 -> line (x1, y1) (x2, y2)))
    |> List.concat
    |> List.sort compare
    |> loop
    |> List.filter (fun u -> List.length u > 1)
    |> List.length
    |> Printf.printf "%i\n"
    