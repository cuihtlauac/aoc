let return x str = Some (x, str)
let (>>=) p f str = Option.bind (p str) (fun (x, str) -> f x str)
let (<|>) p q str = (function None -> Fun.id | x -> Fun.const x) (p str) (q str)
let (<*>) p_f p_x = p_f >>= fun f -> p_x >>= fun x -> return (f x)
let rec kleene p str = str |> (return List.cons <*> p <*> kleene p <|> return [])

let digit seq = match seq () with Seq.Cons (c, seq) when '0' <= c && c <= '9' -> Some (int_of_char c - 48, seq) | _ -> None 
let coma seq = match seq () with Seq.Cons (',', seq) -> Some ((), seq) | _ -> None
let lsqb seq = match seq () with Seq.Cons ('[', seq) -> Some ((), seq) | _ -> None
let rsqb seq = match seq () with Seq.Cons (']', seq) -> Some ((), seq) | _ -> None

type snailfish = Int of int | Pair of snailfish * snailfish
let rec to_string = function
| Int i -> string_of_int i
| Pair (l, r) -> Printf.sprintf "[%s,%s]" (to_string l) (to_string r) 

let rec skip = function
| Int i -> i |> string_of_int |> String.map (fun _ -> ' ')
| Pair (l, r) -> "   " ^ skip l ^ skip r

let integer = digit >>= fun x -> kleene digit >>= fun u -> return (Int (List.fold_left (fun n d -> 10 * n + d) 0 (x :: u)))

let rec snailfish seq = seq |> (integer <|> pair >>= fun x -> return x) 
and pair seq = seq |> (lsqb >>= fun _ -> snailfish >>= fun l -> coma >>= fun _ -> snailfish >>= fun r -> rsqb >>= fun _ -> return (Pair (l, r)))

let parse str = str |> String.to_seq |> snailfish |> Option.get |> fst 

let explode_test = [
  "[[[[[9,8],1],2],3],4]";
  "[7,[6,[5,[4,[3,2]]]]]"; 
  "[[6,[5,[4,[3,2]]]],1]";
  "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]";
  "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
]
let rec zip snail = function
| [] -> snail
| (true, right) :: path -> zip (Pair (snail, right)) path 
| (false, left) :: path -> zip (Pair (left, snail)) path

let rec add_leftmost n = function Int i -> Int (i + n) | Pair (l, r) -> Pair (add_leftmost n l, r)
let rec add_rightmost n = function Int i -> Int (i + n) | Pair (l, r) -> Pair (l, add_rightmost n r)

let rec add_closest_left n = function
| [] -> []
| (false, snail) :: path -> (false, add_rightmost n snail) :: path
| step :: path -> step :: add_closest_left n path

let rec add_closest_right n = function
| [] -> []
| (true, snail) :: path -> (true, add_leftmost n snail) :: path
| step :: path -> step :: add_closest_right n path

let explode snail =
  let rec loop path = function
  | Int _ -> None
  | Pair (l, r) -> match loop ((true, r) :: path) l, loop ((false, l) :: path) r with
    | None, None when List.length path > 3 -> Some (Pair (l, r), path)
    | None, opt -> opt
    | some, _ -> some in
  match loop [] snail with
  Some (Pair (Int i, Int j), zipper) -> zip (Int 0) (zipper |> add_closest_left i |> add_closest_right j)
  | _ -> snail

let split snail =
  let rec loop path = function
  | Int i -> if i < 10 then None else Some (Int i, path)
  | Pair (l, r) -> match loop ((true, r) :: path) l with None -> loop ((false, l) :: path) r | some -> some in
  match loop [] snail with
  | Some (Int n, zipper) -> zip (let n' = n / 2 in Pair (Int n', Int (n - n'))) zipper
  | _ -> snail

let rec iter f n x = if n = 0 then x else iter f (n - 1) (f x)
let rec fix f x = let x' = f x in if x = x' then x else fix f x'

let rec zipper_to_string sf = function 
| [] -> to_string sf
| (true, r) :: path -> " " ^ zipper_to_string sf path ^ " " ^ skip r ^ " "
| (false, l) :: path -> " " ^ skip l ^ " " ^ zipper_to_string sf path ^ " "

let debug_redex (opt, sf) = begin match opt with
| None -> print_newline ()
| Some (n, zip) -> zipper_to_string sf zip ^ string_of_int n |> print_endline end; (opt, sf)

let debug_snailfish sf = print_endline (to_string sf); sf

let f sf =
  let sf' = sf |> explode in 
  if sf = sf' then split sf else sf'

let add a b = fix f (Pair (a, b))
let f = "[[[[4,3],4],4],[7,[[8,4],9]]]" |> String.to_seq |> snailfish |> Option.get |> fst
let g = "[1,1]" |> String.to_seq |> snailfish |> Option.get |> fst

let _ = Pair (f, g)
let fold_test = [
  "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]";
  "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]";
  "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]";
  "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]";
  "[7,[5,[[3,8],[1,4]]]]";
  "[[2,[2,2]],[8,[8,1]]]";
  "[2,9]";
  "[1,[[[9,3],9],[[9,0],[0,7]]]]";
  "[[[5,[7,4]],7],1]";
  "[[[[4,2],2],6],[8,7]]"
]

let rec mag = function
  | Int n -> n
  | Pair (l, r) -> 3 * mag l + 2 * mag r

let mag_test = [
  "[[1,2],[[3,4],5]]";
  "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]";
  "[[[[1,1],[2,2]],[3,3]],[4,4]]";
  "[[[[3,0],[5,3]],[4,4]],[5,5]]";
  "[[[[5,0],[7,4]],[5,5]],[6,6]]";
  "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
]

let f path =
  path
  |> open_in
  |> Seq.unfold Misc.input_line
  |> Seq.map (fun str -> str |> String.to_seq |> snailfish |> Option.get |> fst)
  |> (|>) ()
  |> (function Seq.Nil -> failwith "" | Seq.Cons (x, seq) -> seq |> Seq.fold_left add x |> mag)
  
let _ = Misc.process f 4140

let f path =
  path
  |> open_in
  |> Seq.unfold Misc.input_line
  |> Seq.map (fun str -> str |> String.to_seq |> snailfish |> Option.get |> fst)
  |> List.of_seq
  |> fun u -> List.concat_map (fun x -> List.concat_map (fun y -> if x = y then [] else [(x, y); (y, x)]) u) u
  |> List.map (fun (x, y) -> add x y |> mag)
  |> List.fold_left max min_int

let _ = Misc.process f 3993
