let seq_take4 seq0 = match seq0 () with
| Seq.Nil -> failwith "seq_take4" | Seq.Cons (x0, seq1) -> match seq1 () with
| Seq.Nil -> failwith "seq_take4" | Seq.Cons (x1, seq2) -> match seq2 () with
| Seq.Nil -> failwith "seq_take4" | Seq.Cons (x2, seq3) -> match seq3 () with
| Seq.Nil -> failwith "seq_take4" | Seq.Cons (x3, seq4) -> (x0, x1, x2, x3, seq4)

let bin = List.fold_left (fun b c -> b * 2 + c) 0

let rec take n = function 
| x :: u when n > 0 -> x :: take (n - 1) u
| _ -> []

let to_binary str =
  str
  |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - if c < 'A' then 48 else 55)
  |> Seq.concat_map (fun i -> (Seq.cons ((i / 8) mod 2) (Seq.cons ((i / 4) mod 2) (Seq.cons ((i / 2) mod 2) (Seq.cons (i mod 2) Seq.empty)))))
  |> List.of_seq

let _ = "D2FE28"