type 'a line =
| Mem of int * int
| Mask of 'a

let bin = Seq.fold_left (fun b c -> b * 2 + if c = '1' then 1 else 0) 0
let neg = Seq.fold_left (fun b c -> b * 2 + if c <> '0' then 1 else 0) 0
let read1 s = Mask (bin s, neg s)

module Mem = Map.Make(Int)

let read_line f cin = 
    let line = cin |> input_line in
    try Scanf.sscanf line "mem[%d] = %d" (fun x y -> Mem (x, y))
    with _ -> Scanf.sscanf line "mask = %s" (fun s -> s |> String.to_seq |> f)

let try_read_line f cin =  try Some (read_line f cin, cin) with End_of_file -> None

let read_data f str = str |> open_in |> Seq.unfold (try_read_line f)

let write1 (x, y) address value mem = Mem.add address ((value lor x) land y) mem

let read2 s = Mask (bin s, s |> Seq.fold_left (fun (i, u) c -> (i + 1, if c = 'X' then 35 - i :: u else u)) (0, []) |> snd)

let rec int_fold_left f x i = if i < 0 then x else int_fold_left f (f x i) (i - 1)

let set bit off = (if bit = 0 then fun a -> a |> lnot |> (land) else (lor)) (1 lsl off)
let get off n = 1 land (n lsr off)

let addr bits u n = List.fold_left (fun (i, n) woff -> i + 1, set (get i bits) woff n) (0, n) u |> snd

let write2 (mask, u) address value mem = int_fold_left (fun mem i -> Mem.add (addr i u (address lor mask)) value mem) mem (1 lsl List.length u - 1)

let process write init data =
     Seq.fold_left (fun (mask_u, mem) line -> match line with Mask (x, u) -> ((x, u), mem) | Mem (x, y) -> (mask_u, write mask_u x y mem)) ((0, init), Mem.empty) data |> snd

let output mem = mem |> Mem.bindings |> List.fold_left (fun s (_, v) -> s + v) 0 |> Printf.printf "%d\n"

let _ = 
    "day14.input" |> read_data read1 |> process write1 1 |> output;
    "day14.input" |> read_data read2 |> process write2 [] |> output
