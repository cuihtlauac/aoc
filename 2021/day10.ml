let read_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

type bracket = Round | Squar | Curly | Angle 
type chunk = R of bracket | L of bracket

let to_bracket = function
| '(' -> R Round
| '[' -> R Squar
| '{' -> R Curly
| '<' -> R Angle
| ')' -> L Round
| ']' -> L Squar
| '}' -> L Curly
| '>' -> L Angle
| _ -> failwith "to_bracket"

let step stack c = match stack, to_bracket c with
| Either.Left (R x :: stack), L y when x = y -> Either.Left stack
| Either.Left stack, R x -> Either.Left (R x :: stack)
| Either.Left _, L Round -> Either.Right 3
| Either.Left _, L Squar -> Either.Right 57
| Either.Left _, L Curly -> Either.Right 1197
| Either.Left _, L Angle -> Either.Right 25137
| Either.Right c, _ -> Either.Right c

let _ =
  "day10.data"
  |> open_in
  |> Seq.unfold read_line
  |> Seq.map (fun str ->
    str
    |> String.to_seq
    |> Seq.fold_left step (Either.Left [])
    |> (function Either.Right x -> x | Either.Left _ -> 0)
  )
  |> Seq.fold_left (+) 0
  |> Printf.printf "%i\n"

let _ =
  "day10.data"
  |> open_in
  |> Seq.unfold read_line
  |> Seq.map (fun str ->
    str
    |> String.to_seq
    |> Seq.fold_left step (Either.Left [])
  )
  |> Seq.filter_map (function Either.Right _ -> None | Either.Left stack -> Some (List.map (function L x -> x | R x -> x) stack))
  |> Seq.map (List.fold_left (fun score x -> score * 5 + (function Round -> 1 | Squar -> 2 | Curly -> 3 | Angle -> 4) x) 0)
  |> List.of_seq
  |> List.sort compare
  |> fun u -> List.nth u (List.length u / 2)
  |> Printf.printf "%i\n"