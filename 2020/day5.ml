let bsearch a =
    let rec loop (lo, x) (hi, y) =
        if hi - lo = 2 then a.(lo + 1) + 1
        else
            let mi = (hi + lo) / 2 in
            let z = a.(mi) - mi in
            if x = z then
                loop (mi + 1, a.(mi + 1) - mi - 1) (hi, y)
            else
                loop (lo, x) (mi, z) in
    let hi = Array.length a - 1 in 
    loop (0, a.(0)) (hi, a.(hi) - hi)


let _ =
    let a = "day5.input" |> open_in |> Seq.unfold (fun chan -> try Some (input_line chan, chan) with End_of_file -> None)
    |> Seq.map (fun s -> 1023 - (s |> String.to_seq |> Seq.fold_left (fun b c -> b * 2 + int_of_char c land 4 / 4) 0))
    |> Array.of_seq in
    Array.sort compare a;
    Printf.printf "%d\n%d\n" a.(Array.length a - 1) (bsearch a)
