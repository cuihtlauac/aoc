let input_scanner cin =
  try
  cin
  |> fun c -> (ignore (Stdlib.input_line c); c)
  |> Seq.unfold (fun cin -> cin |> Stdlib.input_line |> (fun str -> try Scanf.sscanf str "%i,%i,%i" (fun a b c -> Some ((a, b, c), cin)) with End_of_file -> None))
  |> fun seq -> Some (List.of_seq seq, cin)
  with End_of_file -> None

let qmult (a1, b1, c1, d1) (a2, b2, c2, d2) = (
  a1 *. a2 -. b1 *. b2 -. c1 *. c2 -. d1 *. d2,
  a1 *. b2 +. b1 *. a2 +. c1 *. d2 -. d1 *. c2,
  a1 *. c2 -. b1 *. d2 +. c1 *. a2 +. d1 *. b2,
  a1 *. d2 +. b1 *. c2 -. c1 *. b2 +. d1 *. a2
)

let qconj ((a: float), b, c, d) = (a, -.b, -.c, -.d)

let qrot q p = qmult (qmult q p) (qconj q)

let rot (x, y, z) a = qrot (cos (a /. 2.0), x *. sin (a /. 2.0), y *. sin (a /. 2.0), z *. sin (a /. 2.0))

let rot2 (x, y) n = match n mod 4 with
| 0 -> (x, y)
| 1 -> (-y, x)
| 2 -> (-x, -y)
| _ -> (y, -x)

let rot (x, y, z) n = match (n mod 24) / 4 with
| 0 -> let x, y = rot2 (x, y) n in (x, y, z)
| 1 -> let x, y = rot2 (x, y) n in (x, y, -z)
| 2 -> let x, z = rot2 (x, z) n in (x, y, z) 
| 3 -> let x, z = rot2 (x, z) n in (x, -y, z) 
| 4 -> let y, z = rot2 (y, z) n in (x, y, z)
| _ -> let y, z = rot2 (y, z) n in (-x, y, z)

let vect_int_of_float (a, b, c, d) = (
  a |> Float.round |> int_of_float,
  b |> Float.round |> int_of_float,
  c |> Float.round |> int_of_float,
  d |> Float.round |> int_of_float
)

let vect_float_of_int (a, b, c, d) = (
  float_of_int a,
  float_of_int b,
  float_of_int c,
  float_of_int d
)


let f path =
  path
  |> open_in
  |> Seq.unfold input_scanner
  |> Fun.const 42  
