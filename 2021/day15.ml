module Cache = Set.Make(struct type t = int * (int * int) let compare = compare end)

let cache_update x x' pos set = set |> Cache.remove (x, pos) |> Cache.add (x', pos)

let rec loop mat visited border =
    if (List.length visited) mod (Array.length mat * Array.length mat / 100) = 0 then prerr_char '.';
    let len, (i, j) = Cache.min_elt border in
    let border = Cache.remove (len, (i, j)) border in
    let visited = ((i, j), len) :: visited in
    if i = Array.length mat - 1 && j = Array.length mat - 1 then
        len
    else 
        let g pos border = 
            match (try Some mat.(fst pos ).(snd pos) with Invalid_argument _ -> None), List.assoc_opt pos visited, Option.map fst (Cache.find_last_opt (fun (_, x) -> x = pos) border) with
            | Some x, None, Some len' when len + x < len' -> border |> cache_update len' len  pos
            | Some x, None, None -> Cache.add (len + x, pos) border
            | _ -> border
            in
            border |> g (i, j - 1) |> g (i, j + 1) |> g (i - 1, j) |> g (i + 1, j) |> loop mat visited
    
let f path =
    let mat = 
        path
        |> open_in
        |> Seq.unfold Misc.input_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
        |> Array.of_seq in
    loop mat [] (Cache.singleton (0, (0, 0)))

let _ = Misc.process f 40

let mat5 mat =
    let len = Array.length mat in
    Array.make_matrix (5 * len) (5 * len) 0
    |> Array.mapi (fun i -> Array.mapi (fun j _ -> (mat.(i mod len).(j mod len) + i / len + j / len - 1) mod 9 + 1))  

let print_matrix =
    Array.iter (fun row -> Array.iter print_int row; print_newline ())

let f path =
    let mat = 
        path
        |> open_in
        |> Seq.unfold Misc.input_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
        |> Array.of_seq in
    loop (mat5 mat) [] (Cache.singleton (0, (0, 0)))

