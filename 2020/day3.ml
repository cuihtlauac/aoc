let rec sprintf00b len n =
    if len = 0 then
        ""
    else
        sprintf00b (len - 1) (n lsr 1) ^ string_of_int (n land 1)


let sprintf0b =
    let rec loop str = function
    | 0 -> "0" ^ str 
    | 1 -> "1" ^ str
    | n when n mod 2 = 0 -> loop ("0" ^ str) (n / 2)
    | n -> loop ("1" ^ str) (n / 2) in
    loop ""

(* '#' = 0b010001_1, '.' = 0b010111_0 *)
let to_int str = str |> String.to_seq |> Seq.fold_left (fun x y -> x * 2 + int_of_char y land 1) 0

let read_line cin = try Some (cin |> Stdlib.input_line |> to_int, cin) with End_of_file -> None

let down len off =
    List.fold_left (fun (s, i) x -> s + x lsr i land 1, (len + i - off) mod len) (0, len - 1)

let rec unzip = function
| x :: y :: u ->
    let even, odd = unzip u in
    x :: even, y :: odd
| u -> u, []

let _ =
    let len = 31 in
    let data = "day3.input" |> open_in |> Seq.unfold read_line |> List.of_seq in
    let a, _ = data |> down len 1 in
    let b, _ = data |> down len 3 in
    let c, _ = data |> down len 5 in
    let d, _ = data |> down len 7 in
    let e, _ = data |> unzip |> fst |> down len 1 in
    Printf.printf "%d\n%d\n" b (a * b * c * d * e)
