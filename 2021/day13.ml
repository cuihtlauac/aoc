let fold sym x = if x < sym then x else 2 * sym - x

let fold (dir, sym) (x, y) = if dir then (fold sym x, y) else (x, fold sym y)

module PairSet = Set.Make(struct type t = int * int let compare = compare end)

let grid h w u = List.fold_left (fun a (i, j) -> let a = Array.copy a in Fun.const a (a.(j).(i) <- true)) (Array.make_matrix h w false) u
let print = Array.iter (fun row -> Array.iter (fun cell -> print_char (if cell then '#' else '.')) row; print_newline ())

let read_input path =
    let f c cin = cin |> Seq.unfold (fun cin -> Option.bind (Misc.input_line cin) (fun (str, cin) -> str |> String.split_on_char c |> function [a; b] -> Some ((a, b), cin) | _ -> None)) in
    let cin = path |> open_in in 
    let data = cin |> f ',' |> Seq.map (fun (a, b) -> int_of_string a, int_of_string b)|> PairSet.of_seq in
    let syms = cin |> f '=' |> Seq.map (fun (a, b) -> a.[String.length a - 1] = 'x', int_of_string b) |> List.of_seq in
    (data, syms)

let f path =
    let data, syms = read_input path in
    PairSet.fold (fun (x, y) set -> PairSet.add (fold (List.hd syms) (x, y)) set) data PairSet.empty

let _ = Misc.process (fun path -> path |> f |> PairSet.cardinal) 17

let f path =
    let data, syms = read_input path in
    List.fold_left (fun set (vert, sym) -> PairSet.fold (fun (x, y) set -> PairSet.add (fold (vert, sym) (x, y)) set) set PairSet.empty) data syms

let _ = "day13.data" |> f |> PairSet.elements |> grid 8 64 |> print