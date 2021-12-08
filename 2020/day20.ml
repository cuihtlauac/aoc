let catch f x = try Some (f x, x) with _ -> None

(* Parse [.#]* string *)
let to_int str = str |> String.to_seq |> Seq.fold_left (fun x y -> x * 2 + int_of_char y land 1) 0

let rec bin_of_int len n = if len = 0 then "0b" else bin_of_int (len - 1) (n lsr 1) ^ string_of_int (n land 1)

let range n = let rec loop u i = if n <= i then loop (i :: u) (i - 1) else u in loop []

(*
type id = int (* 144 = 12 * 12: number of tiles (4 bits). *)
type orientation = int (* 8: D4 dihedral group order (3 bits). *) 
type border = int (* 2^10: 10 bits *)
type direction = int (* 4: E, N, W, S (2 bits). *)
*)
(* data: *)
(* id * direction -> border *)
(* id -> direction -> border *)

(* symmetry: *)
(* orientation * direction * border -> border *)
(* orientation -> direction -> border -> border *)

(* tbd: *)
(* border * direction -> (id * orientation) option *)
(* border -> direction -> (id * orientation) option *)

module IntMap = Map.Make(Int)

(* 36 bit operations. *)

(* shift i |> shift j = shift ((i + j) mod 36) *)
let shift n a (* 36 bits *) = ((a lsl n) lor (a lsr (36 - n))) land (1 lsl 36 - 1)

let rev_36 a (* 36 bits *) =
    let swap_18 a = let m = 0xffffc0000 in ((a land m) lsr 18) lor ((a land (m lsr 18)) lsl 18) in
    let swap_9 a = let m = 0xff803fe00 in ((a land m) lsr 9) lor ((a land (m lsr 9)) lsl 9) in
    let swap_5 a = let m = 0xf0783c1e0 in ((a land m) lsr 5) lor ((a land (m lsr 5)) lsl 5) lor (a land 0x80402010) in
    let swap_2 a = let m = 0xc6633198c in ((a land m) lsr 2) lor ((a land (m lsr 2)) lsl 2) lor (a land 0x80402010) in 
    let swap_1 a = let m = 0xa552a954a in ((a land m) lsr 1) lor ((a land (m lsr 1)) lsl 1) lor (a land 0x80402010) in
    a |> swap_18 |> swap_9 |> swap_5 |> swap_2 |> swap_1

let rev_10 a (* 10 bits *) =
    let swap_5 a = let m = 0x3e0 in ((a land m) lsr 5) lor ((a land (m lsr 5)) lsl 5) in 
    let swap_3 a = let m = 0x318 in ((a land m) lsr 3) lor ((a land (m lsr 3)) lsl 3) lor (a land 0x84) in
    let swap_1 a = let m = 0x252 in ((a land m) lsr 1) lor ((a land (m lsr 1)) lsl 1) lor (a land 0x84) in
    a |> swap_5 |> swap_3 |> swap_1

(** flip is vertical    
  
   
    
     
               *)
let orientation n a (* 36 bits *) = match n with
| 0 -> a                    (* id *)
| 1 -> a |> shift 27        (* rot *)
| 2 -> a |> shift 18        (* rot^2 *)
| 3 -> a |> shift 9         (* rot^3 *)
| 4 -> a |> rev_36 |> shift 10 (* flip *)
| 5 -> a |> rev_36 |> shift 19 (* flip; rot *)
| 6 -> a |> rev_36 |> shift 28 (* flip; rot^2 *)
| 7 -> a |> rev_36 |> shift 2  (* flip; rot^3 *)
| _ -> failwith "orientation"

let crop a (* 36 bits *) = a land 0x3ff

let border n a (* 36 bits *) = a |> orientation n |> crop

let tile u =
      String.init 9 (fun i -> u.(i + 1).[9])
    ^ String.init 9 (fun i -> u.(9).[8 - i])
    ^ String.init 9 (fun i -> u.(8 - i).[0])
    ^ String.init 9 (fun i -> u.(0).[i + 1])

let read_tile cin =
    let str = input_line cin in
    let u = Array.init 10 (fun i -> cin |> input_line) in
    let _ = input_line cin in
    (String.sub str 5 4 |> int_of_string, u |> tile)

let data = "day20.test" |> open_in |> Seq.unfold (catch read_tile) |> List.of_seq

let _ =
    let data = "day20.input" |> open_in |> Seq.unfold (catch read_tile) |> TileMap.of_seq in

    data |> List.map (f data) |> List.filter (fun (_, x) -> List.length x = 2) |> List.fold_left (fun res (id, _) -> res * id) 1