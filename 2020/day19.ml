type ('a, 'b) either = Left of 'a | Right of 'b

let fail_empty f line = if line = "" then raise End_of_file else f line

let rule_line str =
    let n, str = str |> String.split_on_char ':' |> fun u -> List.hd u, u |> List.tl |> List.hd in
    (int_of_string n,
        if str.[1] = '"' then
            Left (str.[2])
        else
            Right (str |> String.split_on_char '|' |> List.map (fun str -> str |> String.split_on_char ' ' |> List.filter_map int_of_string_opt))        
    )

let rec check_rule a str pos = function
| Left x ->
    let res = pos < String.length str && str.[pos] = x in
    ((if res then pos + 1 else pos), res)
| Right u -> check_disj a str pos u
and check_disj a str pos = function
| [] -> (pos, false)
| conj :: u ->
    let new_pos, res = check_conj a str pos conj in
    if res then
        (new_pos, res)
    else
        check_disj a str pos u
and check_conj a str pos = function
| [] -> (pos, true)
| ref :: u ->
    let new_pos, res = check_rule a str pos a.(ref) in
    if res then
        check_conj a str new_pos u
    else
        (pos, res)

let range n = let rec loop u i = if n <= i then loop (i :: u) (i - 1) else u in loop []
let (>>=) u f = List.concat_map f u
let bang n =
    let f k i = List.init i (Fun.const k) in
    range 1 n >>= fun i -> range 1 n >>= fun j -> [f 42 i @ f 42 j @ f 31 j]

let _=
    let chan = open_in "day19.input" in
    let rules = chan |> Seq.unfold (fun cin -> try Some (cin |> input_line |> fail_empty rule_line, cin) with End_of_file -> None) |> List.of_seq in
    let strings = chan |> Seq.unfold (fun cin -> try Some (cin |> input_line, cin) with End_of_file -> None) |> List.of_seq in
    let rules = Array.init (List.length rules) (fun i -> List.assoc i rules) in
    strings |> List.filter (fun str -> let len, res = check_rule rules str 0 rules.(0) in res && len = String.length str) |> List.length |> Printf.printf "%i\n";
    rules.(0) <- Right (bang 6);
    strings |> List.filter (fun str -> let len, res = check_rule rules str 0 rules.(0) in res && len = String.length str) |> List.length |> Printf.printf "%i\n";
