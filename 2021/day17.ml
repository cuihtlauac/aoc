let read str = Scanf.sscanf str "target area: x=%i..%i, y=%i..%i" (fun a b c d -> (a, b, c, d))
let test = read "target area: x=20..30, y=-10..-5" 
let data = read "target area: x=240..292, y=-90..-57"

let fly (min_x, max_x, min_y, _) (x, y, vx, vy) =
  if x < min_x && vx < 0 then None
  else if x > max_x && vx > 0 then None
  else if y < min_y && vy < 0 then None
  else Some ((x, y), (x + vx, y + vy, vx + compare 0 vx, vy - 1))

let list_unfold_rev f =
  let rec loop acc x = match f x with Some (y, x') -> loop (y :: acc) x' | _ -> acc in
  loop []

let is_reach (min_x, max_x, min_y, max_y) (x, y) = min_x <= x && x <= max_x && min_y <= y && y <= max_y
let rec seq_bang x () = Seq.Cons (x, seq_bang x)

let seq_take_rev n = 
    let rec loop acc n seq = match seq () with Seq.Cons (x, seq) when n > 0 -> loop (x :: acc) (n - 1) seq | _ -> acc in
    loop [] n

let flight_path range (vx, vy) = list_unfold_rev (fly range) (0, 0, vx, vy)

let hit range (vx0, vy0) =
  let rec loop (x, y, vx, vy) =
    is_reach range (x, y) || (match fly range (x, y, vx, vy) with Some pos -> loop (snd pos) | _ -> false) in
  loop (0, 0, vx0, vy0)


let heights range n =
  n
  |> Seq.unfold (fun x -> Some (x, x + 1))
  |> Seq.map (fun n -> (n, 1))
  |> Seq.concat_map (Seq.unfold (fun (x, y) -> if x > 0 then Some ((x, y), (x - 1, y + 1)) else None))
  |> Seq.map (flight_path range)
  |> Seq.filter (fun u -> u |> List.hd |> is_reach range) (* BANG! *)
  |> Seq.map (List.map snd)
  |> Seq.map (List.sort (Fun.flip compare))
  |> Seq.map List.hd

(* first star *)
let _ = heights (240, 292, -90, -57) 5 |> seq_take_rev (128 + 32 + 4 + 2) |> List.sort compare

let speeds =
  0
  |> Seq.unfold (fun x -> Some (x, x + 1))
  |> Seq.concat_map (fun n -> Seq.unfold (fun x -> if x >= (-n) then Some ((n - abs x, x), (x - 1)) else None) n)

let rec loop range acc seq n =
  if n = 0 then acc else match seq () with
  | Seq.Nil -> failwith "loop"
  | Seq.Cons (v, seq) -> if hit range v then loop range (v :: acc) seq (n - 1) else loop range acc seq (n - 1)




  let u = [
  (23,-10); (25,-9); (27,-5); (29,-6); (22,-6); (21,-7); (9,0); (27,-7); (24,-5);
  (25,-7); (26,-6); (25,-5); (6,8); (11,-2); (20,-5); (29,-10); (6,3); (28,-7);
  (8,0); (30,-6); (29,-8); (20,-10); (6,7); (6,4); (6,1); (14,-4); (21,-6);
  (26,-10); (7,-1); (7,7); (8,-1); (21,-9); (6,2); (20,-7); (30,-10); (14,-3);
  (20,-8); (13,-2); (7,3); (28,-8); (29,-9); (15,-3); (22,-5); (26,-8); (25,-8);
  (25,-6); (15,-4); (9,-2); (15,-2); (12,-2); (28,-9); (12,-3); (24,-6); (23,-7);
  (25,-10); (7,8); (11,-3); (26,-7); (7,1); (23,-9); (6,0); (22,-10); (27,-6);
  (8,1); (22,-8); (13,-4); (7,6); (28,-6); (11,-4); (12,-4); (26,-9); (7,4);
  (24,-10); (23,-8); (30,-8); (7,0); (9,-1); (10,-1); (26,-5); (22,-9); (6,5);
  (7,5); (23,-6); (28,-10); (10,-2); (11,-1); (20,-9); (14,-2); (29,-7); (13,-3);
  (23,-5); (24,-8); (27,-9); (30,-7); (28,-5); (21,-10); (7,9); (6,6); (21,-5);
  (27,-10); (7,2); (30,-9); (21,-8); (22,-7); (24,-9); (20,-6); (6,9); (29,-5);
  (8,-2); (27,-8); (30,-5); (24,-7)
  ]

let rec pos x seq n = match seq () with
| Seq.Nil -> failwith "pos"
| Seq.Cons (y, seq) -> if x = y then n else pos x seq (n + 1)

(* Snd star *)
let _ = loop (240, 292, -90, -57) [] speeds 2000000 |> List.length