let match_bag str off =
    try
        let start = Str.search_forward (Str.regexp " bags?[.,]?\\( contain \\)?") str off in
        Some (String.sub str off (start - off), Str.match_end ())
    with _ -> None

let match_num str =
    try
        let start = Str.search_backward (Str.regexp "[0-9]+ ") str (String.length str - 1) in
        let stop = Str.match_end () in
        Some (String.sub str start (stop - start) |> String.trim |> int_of_string, String.sub str stop (String.length str - stop))
    with _ -> None

module StringMap = Map.Make(String)

let search colour assoc list =
    let rec loop list = List.fold_left (fun n (_, c) -> n + if c = colour then 1 else loop (StringMap.find c assoc)) 0 list in
    loop list

let rec_assoc colour assoc =
    let rec loop colour = try List.fold_left (fun sum (vol, col) -> sum + (vol * (1 + loop col))) 0 (StringMap.find colour assoc) with _ -> 0 in
    loop colour 

let _ =
    let map =
        "day7.input" |> open_in |> Seq.unfold (fun chan -> try Some (input_line chan, chan) with End_of_file -> None)
        |> Seq.map (fun str -> Seq.unfold (match_bag str) 0 |> List.of_seq |> fun u -> u |> List.hd, u |> List.tl |> List.filter_map match_num)
        |> StringMap.of_seq
    in
    map |> StringMap.bindings |> List.map (fun (_, u) -> u |> search "shiny gold" map) |> List.filter ((<>) 0) |> List.length |> Printf.printf "%d\n";
    rec_assoc "shiny gold" map |> Printf.printf "%d\n"

