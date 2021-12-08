let read_line cin = try Some (cin |> Stdlib.input_line |> int_of_string, cin) with End_of_file -> None
 
let (>>=) u f = u |> List.mapi f |> List.concat

let part1 dico u = u >>= fun _ n -> if dico.(2020 - n) then [n] else []

let part2 dico u = u >>= fun i n -> u >>= fun j m -> if i < j && n + m < 2020 && dico.(2020 - n - m) then [n, m] else []

let _ =
    let data = "day1.input" |> open_in |> Seq.unfold read_line |> List.of_seq in
    let dico = Array.make 2020 false in
    data |> List.iter (fun i -> dico.(i) <- true);
    data |> part1 dico |> List.hd |> (fun n -> Printf.printf "%d\n" (n * (2020 - n)));
    data |> part2 dico |> List.hd |> (fun (n, m) -> Printf.printf "%d\n" (n * m * (2020 - n - m)))