let field = [|
    "departure location", "29-917", "943-952";
    "departure station", "50-875", "884-954";
    "departure platform", "41-493", "503-949";
    "departure track", "50-867", "875-966";
    "departure date", "30-655", "679-956";
    "departure time", "46-147", "153-958";
    "arrival location", "50-329", "344-968";
    "arrival station", "42-614", "623-949";
    "arrival platform", "35-849", "860-973";
    "arrival track", "42-202", "214-959";
    "class", "38-317", "329-968";
    "duration", "44-530", "539-953";
    "price", "28-713", "727-957";
    "route", "30-157", "179-966";
    "row", "38-114", "136-969";
    "seat", "45-441", "465-956";
    "train", "44-799", "824-951";
    "type", "41-411", "437-953";
    "wagon", "39-79", "86-969";
    "zone", "48-306", "317-974"
|]

let ticket = [191;89;73;139;71;103;109;53;97;179;59;67;79;101;113;157;61;107;181;137]

let parse_range str = Scanf.sscanf str "%i-%i" (fun x y -> (x, y))

let try_read_line cin =  try Some (input_line cin, cin) with End_of_file -> None

let read_data str = str |> open_in |> Seq.unfold try_read_line

let invalid u n = Array.fold_left (fun result (_, (a, b), (c, d)) -> result && not (a <= n && n <= b) && not (c <= n && n <= d)) true u

let range m n = let rec loop i u = if m <= i then loop (i - 1) (i :: u) else u in loop n []

let compat n (name, (a, b), (c, d)) = a <= n && n <= b || c <= n && n <= d

module FieldSet = Set.Make(struct
    type t = string * (int * int) * (int * int)
    let compare = compare
end)

let array_filter f a = Array.fold_right (fun x u -> if f x then x :: u else u) a []

let rec resolve u = 
    let singletons = Array.fold_left (fun t s -> if FieldSet.cardinal s = 1 then FieldSet.union s t else t) FieldSet.empty u in
    if FieldSet.cardinal singletons = Array.length u then
        u |> Array.map FieldSet.choose |> Array.to_list
    else
        resolve (Array.map (fun s -> if FieldSet.cardinal s = 1 then s else FieldSet.diff s singletons) u)

let _ =
    let fields = Array.map (fun (name, a, b) -> name, parse_range a, parse_range b) field in
    "day16b.input" |> read_data
    |> Seq.map (fun str -> str |> String.split_on_char ',' |> List.filter_map (fun str -> let n = int_of_string str in if invalid fields n then Some n else None) |> List.fold_left (+) 0)        
    |> Seq.fold_left (+) 0    
    |> Printf.printf "%i\n"

let _ =
    let fields = Array.map (fun (name, a, b) -> name, parse_range a, parse_range b) field in
    "day16b.input" |> read_data |> Seq.map (fun str -> str |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list)
    |> Seq.filter (fun u -> u |> array_filter (invalid fields) |> List.length |> (=) 0) (* remove invalid lines *)
    |> Seq.map (fun u -> u |> Array.map (fun n -> array_filter (compat n) fields |> FieldSet.of_list)) (* Turn each line into list of sets of compatible fields *)
    |> Seq.fold_left (Array.map2 FieldSet.inter) (Array.map (fun _ -> fields |> Array.to_seq |> FieldSet.of_seq) fields)
    |> resolve
    |> List.combine ticket
    |> List.filter (fun (_, (name, _, _)) -> String.sub name 0 3 = "dep")
    |> List.fold_left (fun p (n, _) -> p * n) 1 |> Printf.printf "%i\n"
