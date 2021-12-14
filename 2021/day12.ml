module PathSet = Set.Make(struct type t = string list let compare = compare end)

let is_big node = node.[0] < 'a'

let rec int_unfold f x = let y, x' = f x in if x' = PathSet.empty then y else y + int_unfold f x'

let next g graph path (s', t') =
    let last = List.hd path in
    if last = "end" then (1, PathSet.empty) else
        graph
        |> List.filter_map (fun (a, b) ->
            Option.bind (if a = last then Some b else if b = last then Some a else None)
            (fun fresh -> if is_big fresh || not (List.mem fresh path) || g fresh path then Some (fresh :: path) else None))
        |> List.partition (fun path -> List.hd path = "end")
        |> fun (s, t) -> s' + List.length s, PathSet.add_seq (List.to_seq t) t'

let step g graph set = PathSet.fold (next g graph) set (0, PathSet.empty)

let f g path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.fold_left (fun u str -> str |> String.split_on_char '-' |> function [a; b] -> (a, b) :: u | _ -> failwith "") []
    |> fun graph -> int_unfold (step g graph) (PathSet.singleton ["start"])

let _ = Misc.process (f (fun _ _ -> false)) 10

let no_small_dupes u =
    let u = u |> List.filter (fun x -> not (is_big x) && x <> "start" && x <> "end") in
    try Fun.const true (List.combine u (List.sort_uniq compare u)) with _ -> false

let _ = Misc.process (f (fun fresh path -> fresh <> "start" && no_small_dupes path)) 36