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

let f str = str |> String.to_seq |> Seq.fold_left (fun n c -> n lor (1 lsl (int_of_char c - 97))) 0

let limp x y = (x land y) = x 

let rec length n = if n = 0 then 0 else n land 1 + length (n lsr 1)  

let infer u x =
  if List.find (fun set -> length set = 2) u = x then 1
  else if List.find (fun set -> length set = 7) u = x then 8
  else
  let l7 = u |> List.find (fun set -> length set = 3) in
  let l4 = u |> List.find (fun set -> length set = 4) in
  if l7 = x then 7
  else if l4 = x then 4
  else
  let s096 = u |> List.filter (fun set -> length set = 6) in
  let s325 = u |> List.filter (fun set -> length set = 5) in
  let l3, s25 = s325 |> List.partition (fun set -> limp l7 set) |> fun (u, v) -> (List.hd u, Fun.id v) in
  if l3 = x then 3
  else
  let l9, s06 = s096 |> List.partition (fun set -> limp l4 set) |> fun (u, v) -> (List.hd u, Fun.id v) in
  if l9 = x then 9
  else
  let l0, l6 = s06 |> List.partition (fun set -> limp l7 set) |> fun (u, v) -> (List.hd u, List.hd v) in 
  if l0 = x then 0
  else if l6 = x then 6
  else
  let l5, _ = s25 |> List.partition (fun set -> limp set l9 ) |> fun (u, v) -> (List.hd u, List.hd v) in
  if x = l5 then 5
  else 2

 let f path =
  path
  |> open_in
  |> Seq.unfold Misc.input_line
  |> Seq.map (fun str -> String.sub str 0 58 |> String.split_on_char ' ' |> List.map f, String.sub str 61 (String.length str - 61) |> String.split_on_char ' ' |> List.map f)
  |> Seq.map (fun (u, v) -> List.fold_left (fun x d -> 10 * x + d) 0 (List.map (infer (List.sort_uniq compare (u @ v))) v))
  |> Seq.fold_left (+) 0

let _ = Misc.process f 61229