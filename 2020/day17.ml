let range m n = let rec loop i u = if m <= i then loop (i - 1) (i :: u) else u in loop n []

let (>>=) u f = u |> List.map f |> List.concat

(* '#' = 0b010001_1, '.' = 0b010111_0 *)
let to_int str = str |> String.to_seq |> Seq.fold_left (fun x y -> x * 2 + int_of_char y land 1) 0

let bit_get off n = 1 = 1 land (n lsr off)
let bit_set off bit = (if bit then (lor) else fun a -> a |> lnot |> (land)) (1 lsl off)

let bit_sum n =
    let rec loop s i = if i >= 0 then loop (s + n lsr i land 1) (i - 1) else s in
    loop 0 32

let neighbourhood3 =
    let range = [-1; 0; 1] in
    (range >>= fun i -> range >>= fun j -> range >>= fun k -> if (i, j, k) = (0, 0, 0) then [] else [i, j, k]) |> Array.of_list

(** 
Python:
[ (i, j, k) for i in (-1, 0, 1) for j in (-1, 0, 1) for k in (-1, 0, 1) if (i, j, k) != (0, 0, 0) ]

def neighbourgs(x, y, z) = [ (x + i, y + j, z + k) for i in (-1, 0, 1) for j in (-1, 0, 1) for k in (-1, 0, 1) if (i, j, k) != (0, 0, 0) ]

neighbourgs = lambda x, y, z: [ (x + dx, y + dy, z + dz) for dx in (-1, 0, 1) for dy in (-1, 0, 1) for dz in (-1, 0, 1) if (dx, dz, dz) != (0, 0, 0) ]

[ (i, j, k) for i in (-1, 0, 1) for j in (-1, 0, 1) for k in (-1, 0, 1) if (i, j, k) != (0, 0, 0) ]

flat_map = lambda u, f: [y for v in u for y in f(v)]
flat_map = lambda u, f: itertools.from_iterable(f(u))

flat_map([-1, 0, 1], lambda i: flat_map([-1, 0, 1], lambda j: flat_map([-1, 0, 1], lambda k: [] if (i, j, k) == (0, 0, 0) else [(i, j, k)])))

*)

let neighbourhood4 =
    let range = [-1; 0; 1] in
    (range >>= fun i -> range >>= fun j -> range >>= fun k -> range >>= fun l -> if (i, j, k, l) = (0, 0, 0, 0) then [] else [i, j, k, l]) |> Array.of_list

let grid3_get v (x, y, z) = try bit_get z v.(x).(y) with _ -> false
let grid4_get v (x, y, z, w) = try bit_get w v.(x).(y).(z) with _ -> false
let grid3_set v (x, y, z) b = v.(x).(y) <- bit_set z b v.(x).(y)
let grid4_set v (x, y, z, w) b = v.(x).(y).(z) <- bit_set w b v.(x).(y).(z)
let grid2_sum = Array.fold_left (fun s n -> s + bit_sum n)
let grid3_sum = Array.fold_left grid2_sum
let grid4_sum = Array.fold_left grid3_sum

let sum3 (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)
let sum4 (x0, y0, z0, w0) (x1, y1, z1, w1) = (x0 + x1, y0 + y1, z0 + z1, w0 + w1)

let cell3_next v0 v1 o =
    let s = Array.fold_left (fun s xyz -> s + if grid3_get v0 (sum3 o xyz) then 1 else 0) 0 neighbourhood3 in
    grid3_set v1 o (s = 3 || (grid3_get v0 o && s = 2))

let cell4_next v0 v1 o =
    let s = Array.fold_left (fun s xyzw -> s + if grid4_get v0 (sum4 o xyzw) then 1 else 0) 0 neighbourhood4 in
    grid4_set v1 o (s = 3 || (grid4_get v0 o && s = 2))

let grid3_next n v0 v1 = 
    let u = range 0 (n - 1) in
    (u >>= fun i -> u >>= fun j -> u >>= fun k -> [i, j, k]) |> List.iter (cell3_next v0 v1)
 
let grid4_next n v0 v1 = 
    let u = range 0 (n - 1) in
    (u >>= fun i -> u >>= fun j -> u >>= fun k -> u >>= fun l -> [i, j, k, l]) |> List.iter (cell4_next v0 v1)

let read_line cin = try Some (cin |> Stdlib.input_line |> to_int, cin) with End_of_file -> None

let _ =
    let size = 2 * 6 + 3 in 
    let data = "day17.test" |> open_in |> Seq.unfold read_line |> Seq.map (fun n -> n lsl 5) |> Array.of_seq in
    let grid0, grid1 = Array.make_matrix size size 0, Array.make_matrix size size 0 in
    for i = 0 to Array.length data - 1 do grid0.((size - 1) / 2).(i + 5) <- data.(i) done;
    grid3_next size grid0 grid1; grid3_next size grid1 grid0;
    grid3_next size grid0 grid1; grid3_next size grid1 grid0;
    grid3_next size grid0 grid1; grid3_next size grid1 grid0;
    grid3_sum 0 grid0 

let _ =
    let size = 2 * 6 + 8 in 
    let data = "day17.input" |> open_in |> Seq.unfold read_line |> Seq.map (fun n -> n lsl 5) |> Array.of_seq in
    let grid0, grid1 = Array.make_matrix size size 0, Array.make_matrix size size 0 in
    for i = 0 to Array.length data - 1 do grid0.((size - 1) / 2).(i + 5) <- data.(i) done;
    grid3_next size grid0 grid1; grid3_next size grid1 grid0;
    grid3_next size grid0 grid1; grid3_next size grid1 grid0;
    grid3_next size grid0 grid1; grid3_next size grid1 grid0;
    grid3_sum 0 grid0 |> Printf.printf "%i\n"

let make_matrix3 a b c x =
    Array.init a (fun _ -> Array.init b (fun _ -> Array.make c x))

let _ =
    let size = 2 * 6 + 3 in 
    let data = "day17.test" |> open_in |> Seq.unfold read_line |> Seq.map (fun n -> n lsl 5) |> Array.of_seq in
    let grid0, grid1 = make_matrix3 size size size 0, make_matrix3 size size size 0 in
    for i = 0 to Array.length data - 1 do grid0.((size - 1) / 2).((size - 1) / 2).(i + 5) <- data.(i) done;
    grid4_next size grid0 grid1; grid4_next size grid1 grid0;
    grid4_next size grid0 grid1; grid4_next size grid1 grid0;
    grid4_next size grid0 grid1; grid4_next size grid1 grid0;
    grid4_sum 0 grid0 

let _ =
    let size = 2 * 6 + 8 in 
    let data = "day17.input" |> open_in |> Seq.unfold read_line |> Seq.map (fun n -> n lsl 5) |> Array.of_seq in
    let grid0, grid1 = make_matrix3 size size size 0, make_matrix3 size size size 0 in
    for i = 0 to Array.length data - 1 do grid0.((size - 1) / 2).((size - 1) / 2).(i + 5) <- data.(i) done;
    grid4_next size grid0 grid1; grid4_next size grid1 grid0;
    grid4_next size grid0 grid1; grid4_next size grid1 grid0;
    grid4_next size grid0 grid1; grid4_next size grid1 grid0;
    grid4_sum 0 grid0 |> Printf.printf "%i\n"