let bin = List.fold_left (fun b c -> b * 2 + c) 0

let rec take n = function 
| x :: u when n > 0 -> x :: take (n - 1) u
| _ -> []

let rec take_while p = function
| x :: u when p x -> x :: take_while p u
| _ -> [] 

let rec seq_take n seq = match seq () with
| Seq.Cons (x, seq) when n > 0 -> Seq.cons x (seq_take (n - 1) seq)
| _ -> Seq.empty

let rec seq_take_while p seq = match seq () with
| Seq.Cons (x, seq) when p x -> Seq.cons x (seq_take_while p seq)
| _ -> Seq.empty

let string_to_int_seq str =
  str
  |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - if c < 'A' then 48 else 55)
  |> Seq.concat_map (fun i -> (Seq.cons ((i / 8) mod 2) (Seq.cons ((i / 4) mod 2) (Seq.cons ((i / 2) mod 2) (Seq.cons (i mod 2) Seq.empty)))))

let a = "D2FE28" (* literal 2021 *)
let b = "38006F45291200"
let c = "EE00D40C823060"
let d = "8A004A801A8002F478"
let e = "620080001611562C8802118E34"
let f = "C0015000016115A2E0802F182340"
let g = "A0016C880162017C3686B18A3D4780"


let return x str = Some (x, str)
let (>>=) p f str = Option.bind (p str) (fun (x, str) -> f x str)
let (<|>) p q str = (function None -> Fun.id | x -> Fun.const x) (p str) (q str)
let (<*>) p_f p_x = p_f >>= fun f -> p_x >>= fun x -> return (f x)
let rec kleene p str = str |> (return List.cons <*> p <*> kleene p <|> return [])
let bit seq = match seq () with Seq.Cons (x, seq) -> Some (x, seq) | _ -> None
let zero seq = match seq () with Seq.Cons (0, seq) -> Some (0, seq) | _ -> None
let one seq = match seq () with Seq.Cons (1, seq) -> Some (1, seq) | _ -> None

let nibble seq = match seq () with Seq.Cons (c, seq) -> Some ((int_of_char c - if c < 'A' then 48 else 55), seq) | _ -> None
let nibble str = str |> (bit >>= fun a -> bit >>= fun b -> bit >>= fun c -> bit >>= fun d -> return [a; b; c; d])
let lcons c b = c * 2 + b 

let rec take n = if n > 0 then return lcons <*> take (n - 1) <*> bit else return 0

let group = kleene (one >>= fun _ -> take 4) >>= fun u -> zero >>= fun _ -> take 4 >>= fun x -> return (x :: u) 

let packet = group >>= fun u -> kleene zero >>= fun _ -> return u

let _ = a |> string_to_int_seq |> (take 3 >>= fun a -> take 3 >>= fun b -> kleene (one >>= fun _ -> take 4) >>= fun u -> kleene zero >>= fun _ -> take 4 >>= fun x -> kleene zero >>= fun _ -> return (a, b, u, x))

let hd_if p seq = match seq () with
| Seq.Cons (x, seq) when p x -> Some (x, seq)
| _ -> None
let rec take_while p = return List.cons <*> hd_if p <*> take_while p <|> return []
let bit3 = bit >>= fun a -> bit >>= fun b -> bit >>= fun c -> return (bin [a; b; c])

