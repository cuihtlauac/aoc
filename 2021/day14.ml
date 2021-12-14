let rec iter f n x = if n = 0 then x else iter f (n - 1) (f x)

let step rules u =
    let rec loop u acc = match u with
    | [] -> List.rev acc
    | [c] -> loop [] (c :: acc)
    | cl :: cr :: u -> List.assoc_opt (cl, cr) rules |> (function Some u -> List.nth u 1 :: cl :: acc | _ -> cl :: acc) |> loop (cr :: u)
    in loop u []

let idx c = int_of_char c - 65

let effect f x = f x; x
(*
let del_char a i = let a = Array.copy a in Fun.const a (a.(i) <- a.(i) - 1)
let add_char a i = let a = Array.copy a in Fun.const a (a.(i) <- a.(i) + 1)
*)
let del_char a i = a.(i) <- a.(i) - 1; a
let add_char a i = a.(i) <- a.(i) + 1; a

let add_string = List.fold_left add_char

let f n path =
    let cin = open_in path in
    let start = cin |> Misc.input_line |> Option.get |> fst |> String.to_seq |> Seq.map idx |> List.of_seq in
    let _ = Misc.input_line cin in
    let rules = cin |> Seq.unfold Misc.input_line |> Seq.map (fun str -> Scanf.sscanf str "%c%c -> %c" (fun a b c -> ((idx a, idx b), [idx a; idx c; idx b]))) |> List.of_seq in
    start
    |> iter (step rules) n
    |> add_string (Array.make 26 0)
    |> Array.to_list
    |> List.filter ((<>) 0)
    |> List.sort compare
    |> fun u -> List.nth u (List.length u - 1) - List.nth u 0

let _ = Misc.process (f 10) 1588

let rec weight rules acc n u =
    if n = 0 then
        add_string acc u
    else match u with
    | a :: b :: u -> 
        let acc = del_char acc b in
        let acc = match List.assoc_opt (a, b) rules with
            | Some fresh -> weight rules acc (n - 1) fresh
            | None -> add_string acc [a; b] in
        weight rules acc n (b :: u) 
    | u -> add_string acc u

let rec weight' rules n =
    if n = 0 then List.map (fun (redex, fresh) -> (redex, add_string (Array.make 26 0) fresh)) rules else
    List.map (fun (redex, fresh) ->
        let rules' = weight' rules (n - 1) in
        let left = try List.assoc (List.nth fresh 0, List.nth fresh 1) rules' with _ -> add_string (Array.make 26 0) fresh in
        let right = try List.assoc (List.nth fresh 1, List.nth fresh 2) rules' with _ -> add_string (Array.make 26 0) fresh in
        (redex, del_char (Array.map2 (+) left right) (List.nth fresh 1)) 
    ) rules
        

let f n path =
    let cin = open_in path in
    let start = cin |> Misc.input_line |> Option.get |> fst |> String.to_seq |> Seq.map idx |> List.of_seq in
    let _ = input_line cin in
    let rules = cin |> Seq.unfold Misc.input_line |> Seq.map (fun str -> Scanf.sscanf str "%c%c -> %c" (fun a b c -> ((idx a, idx b), [idx a; idx c; idx b]))) |> List.of_seq in
    start
    |> weight rules (Array.make 26 0) n
    |> Array.to_list
    |> List.filter ((<>) 0)
    |> List.sort compare
    |> fun u -> List.nth u (List.length u - 1) - List.nth u 0

let _ = Misc.process (f 10) 1588

let _ = Printf.printf "%i\n" (f 20 "day14.data")
(* let _ = process (f 40) 2188189693529 *)
