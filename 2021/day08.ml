let f path =
  path
  |> open_in
  |> Seq.unfold Misc.input_line
  |> Seq.map (fun str ->
    String.sub str 61 (String.length str - 61)
    |> String.split_on_char ' '
    |> List.filter_map (fun str -> if String.length str <= 4 || String.length str = 7 then Some str else None)
  )
  |> Seq.fold_left (fun n u -> n + List.length u) 0

let _ = Misc.process f 26

let limp x y = (x land y) = x 

let rec length n = if n = 0 then 0 else n land 1 + length (n lsr 1)  

let infer u x =
  let test_length n set = length set = n in
  if x = List.find (test_length 2) u then 1
  else if x = List.find (test_length 7) u then 8
  else let l7 = u |> List.find (test_length 3) in if x = l7 then 7
  else let l4 = u |> List.find (test_length 4) in if x = l4 then 4
  else let l3, s25 = u |> List.filter (test_length 5) |> List.partition (limp l7) in if [x] = l3 then 3
  else let l9, s06 = u |> List.filter (test_length 6) |> List.partition (limp l4) in if [x] = l9 then 9
  else let l0, l6 = List.partition (limp l7) s06 in if [x] = l0 then 0
  else if [x] = l6 then 6
  else if x = List.find (Fun.flip limp (List.hd l9)) s25 then 5
  else 2

 let f path =
  let g str = str |> String.split_on_char ' ' |> List.map (fun str -> str |> String.to_seq |> Seq.fold_left (fun n c -> n lor (1 lsl (int_of_char c - 97))) 0) in
  path
  |> open_in
  |> Seq.unfold Misc.input_line
  |> Seq.map (fun str -> String.sub str 0 58 |> g, String.sub str 61 (String.length str - 61) |> g)
  |> Seq.map (fun (u, v) -> List.fold_left (fun x d -> 10 * x + d) 0 (List.map (infer (List.sort_uniq compare (u @ v))) v))
  |> Seq.fold_left (+) 0

let _ = Misc.process f 61229