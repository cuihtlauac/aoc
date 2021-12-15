let rec iter f n x = if n = 0 then x else iter f (n - 1) (f x)

let step rules u =
    let rec loop u acc = match u with
    | [] -> List.rev acc
    | [c] -> loop [] (c :: acc)
    | cl :: cr :: u -> List.assoc_opt (cl, cr) rules |> (function Some u -> List.nth u 1 :: cl :: acc | _ -> cl :: acc) |> loop (cr :: u)
    in loop u []

let idx c = int_of_char c - 65

let del_char a i = let a = Array.copy a in Fun.const a (a.(i) <- a.(i) - 1)
let add_char a i = let a = Array.copy a in Fun.const a (a.(i) <- a.(i) + 1)

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

let rec weight' rules n =
    if n = 0 then List.map (fun ((a, b), _) -> ((a, b), add_string (Array.make 26 0) [a; b])) rules else
        let rules' = weight' rules (n - 1) in
    List.map (fun (redex, fresh) ->        
        let left = try List.assoc (List.nth fresh 0, List.nth fresh 1) rules' with _ -> add_string (Array.make 26 0) fresh in
        let right = try List.assoc (List.nth fresh 1, List.nth fresh 2) rules' with _ -> add_string (Array.make 26 0) fresh in
        (redex, del_char (Array.map2 (+) left right) (List.nth fresh 1)) 
    ) rules

let rec weight'' rules = function
| a :: b :: u ->
    del_char (Array.map2 (+) (try List.assoc (a, b) rules with _ -> add_string (Array.make 26 0) [a; b]) (weight'' rules (b :: u))) b
| u -> add_string (Array.make 26 0) u

let f n path =
    let cin = open_in path in
    let start = cin |> Misc.input_line |> Option.get |> fst |> String.to_seq |> Seq.map idx |> List.of_seq in
    let _ = Misc.input_line cin in
    let rules = cin |> Seq.unfold Misc.input_line |> Seq.map (fun str -> Scanf.sscanf str "%c%c -> %c" (fun a b c -> ((idx a, idx b), [idx a; idx c; idx b]))) |> List.of_seq in
    let rules' = weight' rules n in
    start
    |> weight'' rules'
    |> Array.to_list
    |> List.filter ((<>) 0)
    |> List.sort compare
    |> fun u -> List.nth u (List.length u - 1) - List.nth u 0

let _ = Misc.process (f 10) 1588

let _ = Misc.process (f 40) 2188189693529
