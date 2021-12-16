let input_line cin = try Some (Stdlib.input_line cin, cin) with End_of_file -> None

let rec span f acc = function
| x :: u when f x -> span f (x :: acc) u
| u -> acc, u

let process f x =
  let day = Sys.argv.(0) |> Filename.basename |> Filename.remove_extension in
  begin if Array.length Sys.argv > 1 then
      if f (day ^ ".test") = x then "PASS" else "FAIL"
  else
      day ^ ".data" |> f |> string_of_int end
  |> fun str -> Printf.printf "%s\n" str; flush stdout
