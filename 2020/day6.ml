let read_line cin = try Some (cin |> Stdlib.input_line, cin) with End_of_file -> None

let rec group buf input = function
| [] -> input :: buf
| "" :: u -> group (input :: buf) [] u
| x :: u -> group buf (String.split_on_char ' ' x @ input) u

module CharSet = Set.Make(Char)

let _ =
    "day6.input" |> open_in |> Seq.unfold read_line |> List.of_seq
    |> group [] []
    |> List.map (fun u -> u |> List.map (fun str -> str |> String.to_seq |> CharSet.of_seq))
    |> List.map (fun u -> List.fold_left CharSet.inter (List.hd u) (List.tl u) |> CharSet.cardinal)
    |> List.fold_left (+) 0
    |> Printf.printf "%d\n"
