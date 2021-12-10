let _ = 
  let data = 
    "day07.data"
    |> open_in
    |> input_line
    |> String.split_on_char ','
    |> List.map int_of_string
    |> List.sort compare in
  let median = List.nth data (List.length data / 2) in
  data
  |> List.fold_left (fun s n -> s + abs (n - median)) 0
  |> Printf.printf "%i\n"

let rec search_min f lo hi =
  if lo = hi then f lo else
    let lo' = (2 * lo + hi + 1) / 3 in
    let hi' = (lo + 2 * hi + 1) / 3 in
    match compare (f lo') (f hi') with
    | x when x > 0 -> search_min f lo' hi
    | x when x < 0 -> search_min f lo hi'
    | _ -> search_min f lo' hi'

let _ =
  let data = 
    "day07.data"
    |> open_in
    |> input_line
    |> String.split_on_char ','
    |> List.map int_of_string
    |> List.sort compare in
  search_min (fun x -> List.fold_left (fun s n -> let d = abs (n - x) in s + d * (d + 1) / 2) 0 data) 0 (List.length data - 1)
  |> Printf.printf "%i\n"
