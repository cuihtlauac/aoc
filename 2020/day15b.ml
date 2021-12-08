let rec iter n f x = if n = 0 then x else iter (n - 1) f (f x)

module IntSplay = Splay.Make(struct type t = int let default = 0 let compare = compare end)(struct type t = int let default = 0 end)

let next (x, (size, t)) =
  let y = try size - IntSplay.find t x with Not_found -> 0 in
  y, (size + 1, (IntSplay.set t x size; t))

let init u =
  let u = List.rev u in
  List.hd u, List.fold_right (fun n (i, t) -> i + 1, (IntSplay.set t n i; t)) (List.tl u) (1, IntSplay.create ())

let _ = 
    let data = [1; 12; 0; 20; 8; 16] in
    data |> init |> iter (2020 - List.length data) next |> fst |> Printf.printf "%d\n";
    data |> init |> iter (30_000_000 - List.length data) next |> fst |> Printf.printf "%d\n"
