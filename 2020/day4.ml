let read_line cin = try Some (cin |> Stdlib.input_line, cin) with End_of_file -> None

let rec group buf input = function
| [] -> buf
| "" :: u -> group (input :: buf) [] u
| x :: u -> group buf (String.split_on_char ' ' x @ input) u

let byr_valid s = try let x = int_of_string s in 1920 <= x && x <= 2002 with _ -> false
let iyr_valid s = try let x = int_of_string s in 2010 <= x && x <= 2020 with _ -> false
let eyr_valid s = try let x = int_of_string s in 2020 <= x && x <= 2030 with _ -> false
let hgt_valid s =
    try
        let unyt = String.sub s (String.length s - 2) 2 in
        let size = String.sub s 0 (String.length s - 2) |> int_of_string in
        unyt = "cm" && 150 <= size && size <= 193 || unyt = "in" && 59 <= size && size <= 76
    with _ -> false
let hcl_valid s =
    try
        s.[0] = '#' && String.length s = 7 && String.sub s 1 (String.length s - 1) |> String.to_seq |> List.of_seq |> List.fold_left (fun b c -> b && ('0' <= c && c <= '9' || 'a' <= c && c <= 'f')) true
    with _ -> false
let ecl_valid s = s = "amb" || s = "blu" || s = "brn" || s = "gry" || s = "grn" || s = "hzl" || s = "oth"
let pid_valid s = 
    String.length s = 9 && (s |> String.to_seq |> List.of_seq |> List.fold_left (fun b c -> b && '0' <= c && c <= '9') true)

let rec fields (byr, iyr, eyr, hgt, hcl, ecl, pid) = function
| ("byr", str) :: u -> fields (byr_valid str, iyr, eyr, hgt, hcl, ecl, pid) u
| ("iyr", str) :: u -> fields (byr, iyr_valid str, eyr, hgt, hcl, ecl, pid) u
| ("eyr", str) :: u -> fields (byr, iyr, eyr_valid str, hgt, hcl, ecl, pid) u
| ("hgt", str) :: u -> fields (byr, iyr, eyr, hgt_valid str, hcl, ecl, pid) u
| ("hcl", str) :: u -> fields (byr, iyr, eyr, hgt, hcl_valid str, ecl, pid) u
| ("ecl", str) :: u -> fields (byr, iyr, eyr, hgt, hcl, ecl_valid str, pid) u
| ("pid", str) :: u -> fields (byr, iyr, eyr, hgt, hcl, ecl, pid_valid str) u
| _ :: u -> fields (byr, iyr, eyr, hgt, hcl, ecl, pid) u
| [] -> (byr, iyr, eyr, hgt, hcl, ecl, pid)

let _ =
    let data = "day4.input" |> open_in |> Seq.unfold read_line |> List.of_seq |> group [] []
    |> List.map (List.map (fun s -> s |> String.split_on_char ':' |> fun u -> (List.nth u 0, List.nth u 1))) in
    data |> List.map (fun u -> u |> List.filter (fun (n, _) -> n <> "cid") |> List.length) |> List.filter ((=) 7) |> List.length |> Printf.printf "%d\n";
    data |> List.map (fields (false, false, false, false, false, false, false))   
    |> List.map (fun (byr, iyr, eyr, hgt, hcl, ecl, pid) -> byr && iyr && eyr && hgt && hcl && ecl && pid)
    |> List.filter Fun.id
    |> List.length
    |> Printf.printf "%d\n";
