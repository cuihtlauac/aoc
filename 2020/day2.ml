let parse_line cin = (fun x y char passwd -> x, y, char, passwd |> String.to_seq |> List.of_seq) |> Scanf.bscanf cin "%d-%d %c: %s\n"

let read_line cin = try Some (cin |> parse_line, cin) with End_of_file -> None

let part1 (x, y, char, passwd) =
    let count = passwd |> List.filter ((=) char) |> List.length in
    x <= count && count <= y 

let part2 (x, y, char, passwd) =
    let x = List.nth passwd (x - 1) in
    let y = List.nth passwd (y - 1) in
    x = char && y <> char || x <> char && y = char

let _ = 
    let data = "day2.input" |> Scanf.Scanning.open_in |> Seq.unfold read_line |> List.of_seq in
    data |> List.filter part1 |> List.length |> Printf.printf "%d\n";
    data |> List.filter part2 |> List.length |> Printf.printf "%d\n"