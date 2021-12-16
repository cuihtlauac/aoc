let (>>=) u f = List.concat_map f u

let rec list_unfold f x = match f x with Some (y, x') -> y :: list_unfold f x' | _ -> []

let diags len =
    let a = List.init len (fun j -> (0, j)) >>= list_unfold (fun (i, j) -> if j >= 0 then Some ((i, j), (i + 1, j - 1)) else None) in
    let b = List.init (len - 1) (fun i -> (i + 1, len - 1)) >>= list_unfold (fun (i, j) -> if i < len then Some ((i, j), (i + 1, j - 1)) else None) in
    a @ b

let def f x y z = if x = z then y else f z

module Cache = Set.Make(struct type t = (int * int) * int let compare (_, a) (_, b) = compare a b end)

let rec loop mat visited border =
    let (i, j), len = try Cache.min_elt border with | e -> Printf.printf "bang!"; raise e in
    let border = Cache.remove ((i, j), len) border in
    Printf.printf "%i %i: \n" i j;
    if i = Array.length mat - 1 && j = Array.length mat - 1 then
        len
    else
        let visited = ((i, j), len) :: visited in
        let g pos border = match (try Some mat.(i).(j) with _ -> None), List.assoc_opt pos visited, (Cache.find_last_opt (fun (x, _) -> x = pos) border) with
        | None,   _,      _ -> border (* not in mat *)
        | _,      Some _, _  -> border (* already visited *)
        | Some x, _,      Some (_, len') when len + x < len' -> border |> Cache.remove (pos, len') |> Cache.add (pos, len + x)
        | Some _, None,   Some (_, _) -> border (* not better *)
        | Some x, _,      None -> border |> Cache.add (pos, len + x) 
    in
        let border = g (i, j - 1) border in
        let border = g (i, j + 1) border in
        let border = g (i - 1, j) border in
        let border = g (i + 1, j) border in
        loop mat visited border

let f path =
    let mat = 
        path
        |> open_in
        |> Seq.unfold Misc.input_line
        |> Seq.map (fun str -> str |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
        |> Array.of_seq in
    loop mat [] (Cache.singleton ((0, 0), mat.(0).(0)))
    |> fun n -> n - mat.(0).(0)

let _ = Misc.process f 40
