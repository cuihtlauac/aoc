let read_line () = try Some (() |> read_line, ()) with End_of_file -> None

let _ =
    () |> Seq.unfold read_line |> List.of_seq 