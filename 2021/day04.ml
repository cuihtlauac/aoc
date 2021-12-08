let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

let f cin = cin |> Stdlib.input_line |> String.split_on_char ' ' |> List.map String.trim |> List.filter ((<>) "") |> List.map (fun x -> x |> int_of_string |> Either.left)

let read_board cin = try 
    let _ = read_line cin in
    let a = f cin in
    let b = f cin in
    let c = f cin in
    let d = f cin in
    let e = f cin in
    Some([a; b; c; d; e], cin)
    with _ -> None

let mark n board = List.map (List.map (function Either.Left x when x = n -> Either.Right x | x -> x)) board

let transp m =
  let rec loop acc = function
    | [] -> List.rev acc
    | [] :: _ -> List.rev acc
    | u -> loop (List.map List.hd u :: acc) (List.map List.tl u)
  in loop [] m

let win_row row = List.for_all Either.is_right row

let win board = List.exists win_row board || List.exists win_row (transp board)

let score i w = w |> List.map (List.filter Either.is_left) |> List.concat |> List.map (function Either.Left n -> n | _ -> failwith "loop") |> List.fold_left (+) 0  |> ( * ) i

let rec loop boards i u = match List.find_opt win boards, u with
    | Some w, _ -> score i w
    | None, i :: rnd -> loop (List.map (mark i) boards) i rnd 
    | _, [] -> failwith "loop"
    
let _ =
    let input = "day04.data" |> open_in in 
    let rnd = input |> read_line |> Option.map (fun x -> x |> fst |> String.split_on_char ',') |> Option.get |> List.map int_of_string in
    let boards = input |> Seq.unfold read_board |> List.of_seq in
    Printf.printf "%i\n" (loop boards min_int rnd) 

let rec loop2 winners  (boards : (int, 'a) Either.t list list list) = function
  | [] -> winners |> List.hd |> (fun (w, i) -> score i (List.hd w))
  | i :: rnd ->
    let w, boards = boards |> List.map (mark i) |> List.partition win in
    if List.length w > 0 then
      loop2 ((w, i) :: winners) boards rnd
    else 
      loop2 winners boards rnd
  
let _ =
    let input = "day04.data" |> open_in in 
    let rnd = input |> read_line |> Option.map (fun x -> x |> fst |> String.split_on_char ',') |> Option.get |> List.map int_of_string in
    let boards = input |> Seq.unfold read_board |> List.of_seq in
    Printf.printf "%i\n" (loop2 [] boards rnd) 