let f path =
    let data = path
    |> open_in
    |> Seq.unfold Misc.input_line
    |> List.of_seq 
    |> List.rev in
    let y, data = data |> List.hd |> fun str -> String.sub str 13 (String.length str - 13) |> int_of_string, List.tl data in
    let x, data = data |> List.hd |> fun str -> String.sub str 13 (String.length str - 13) |> int_of_string, List.tl data in
    let _ = List.tl data in
    failwith "TODO"


let _ = Misc.process f 17
