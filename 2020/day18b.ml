type 'a parser = char list -> ('a * char list) option

let return x str = Some (x, str)
let (>>=) p f str = Option.bind (p str) (fun (x, str) -> f x str)
let (<|>) p q str = (function None -> Fun.id | x -> Fun.const x) (p str) (q str)
let (<*>) p_f p_x = p_f >>= fun f -> p_x >>= fun x -> return (f x)

let char c = function x :: str when x = c -> Some (x, str) | _ -> None

let rec kleene p str = str |> (return List.cons <*> p <*> kleene p <|> return [])
let token p = kleene (char ' ') >>= fun _ -> p >>= fun x -> kleene (char ' ') >>= fun _ -> return x
let group p = token (char '(') >>= fun _ -> p >>= fun x -> token (char ')') >>= fun _ -> return x
let digit = (char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9') >>= fun c -> return (int_of_char c - 48)
let nat = token (return (List.fold_left (fun n -> ( + ) (n * 10))) <*> digit <*> kleene digit)
let fn c f = char c >>= fun _ -> return f

let rec expr1 str = str |> (term1 >>= tail1)
    and tail1 a = ((fn '+' (( + ) a) <|> fn '*' (( * ) a)) <*> term1 >>= tail1) <|> return a
    and term1 str = str |> (nat <|> group expr1)

let rec expr2 str = str |> (fact2 >>= fun a -> fn '*' (( * ) a) <*> expr2 <|> return a)
    and fact2 str = str |> (term2 >>= fun a -> fn '+' (( + ) a) <*> fact2 <|> return a)
    and term2 str = str |> (nat <|> group expr2)

let rec seq_hylo f g z x = match f x with Some (y, x) -> seq_hylo f g (g z y) x | None -> z

let _ = 
    "day18.input" |> open_in
    |> seq_hylo
        (fun cin -> try Some (cin |> input_line |> String.to_seq |> List.of_seq, cin) with _ -> None)
        (fun (m, n) s -> m + fst (Option.get (expr1 s)), n + fst (Option.get (expr2 s)))
        (0, 0)
    |> fun (x, y) -> Printf.printf "%d\n%d\n" x y
