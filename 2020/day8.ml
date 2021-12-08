module IntSet = Set.Make(Int)

let read_line cin = try Some (Scanf.bscanf cin "%s %d\n" (fun x y -> (x, y)), cin) with End_of_file -> None

let rec execute code seen acc step =
    try
        if IntSet.mem step seen then
            (false, acc)
        else
            let ins, arg = code.(step) in            
            execute code (IntSet.add step seen) (acc + if ins = "acc" then arg else 0) (step + if ins = "jmp" then arg else 1)
    with _ -> (true, acc)

let _ =
    let code = "day8.input" |> Scanf.Scanning.open_in |> Seq.unfold read_line |> Array.of_seq in
    Printf.printf "%d\n" (execute code IntSet.empty 0 0 |> snd);
    let rec loop i =
        let ins, arg = code.(i) in
        let status, acc = match ins with
        | "jmp" -> code.(i) <- ("nop", arg); execute code IntSet.empty 0 0
        | "nop" -> code.(i) <- ("jmp", arg); execute code IntSet.empty 0 0
        | _ -> false, (-1) in 
        if status then acc else begin code.(i) <- (ins, arg); loop (i + 1) end in
    loop 0 |> Printf.printf "%d\n"
