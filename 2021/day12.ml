module PathSet = Set.Make(struct type t = string list let compare = compare end)

let rec pathset_fold f x = let y, x' = f x in if x' = PathSet.empty then y else PathSet.union y (pathset_fold f x')

let is_big node = node.[0] < 'a'

let next g path graph =
    let last = List.hd path in
    if last = "end" then (PathSet.singleton path, PathSet.empty) else
        graph
        |> List.filter_map (fun (a, b) ->  if a = last then Some b else if b = last then Some a else None)
        |> List.filter_map (fun fresh -> if is_big fresh || not (List.mem fresh path) || g fresh path then Some (fresh :: path) else None)
        |> List.partition (fun path -> List.hd path = "end")
        |> fun (s, t) -> PathSet.of_list s, PathSet.of_list t

let step g graph set = PathSet.fold (fun path (s, t) -> let s', t' = next g path graph in PathSet.union s s', PathSet.union t t') set (PathSet.empty, PathSet.empty)

let f g path =
    path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> Seq.fold_left (fun u str -> str |> String.split_on_char '-' |> function [a; b] -> (a, b) :: u | _ -> failwith "") []
    |> fun graph -> pathset_fold (step g graph) (PathSet.singleton ["start"])
    |> PathSet.cardinal

let _ = Misc.process (f (fun _ _ -> false)) 10

let no_small_dupes u =
    let u = u |> List.filter (fun x -> not (is_big x) && x <> "start" && x <> "end") in
    try Fun.const true (List.combine u (List.sort_uniq compare u)) with _ -> false

let _ = Misc.process (f (fun fresh path -> fresh <> "start" && no_small_dupes path)) 36