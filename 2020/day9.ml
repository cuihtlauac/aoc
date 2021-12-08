
let read_line cin = try Some (Scanf.bscanf cin "%d\n" Fun.id, cin) with End_of_file -> None

type state = {
    length : int;
    mutable pos : int;
    cbuf : int array array
}

let init slide_length data = let length = slide_length - 1 in {
        length = length ;
        pos = 0;
        cbuf = Array.init length (fun i -> Array.init length (fun j -> data.(i) + data.(length + i - j)))
    }

let update data state offset =
    for i = 0 to state.length - 1 do
        state.cbuf.(state.pos).(i) <- data.(offset - 1) + data.(offset + state.length - i - 1)
    done;
    state.pos <- (state.pos + 1) mod state.length

let is_sum state x =
    let result = ref false in 
    for i = 0 to state.length - 1 do
        for j = i to state.length - 1 do
            result := !result || x = state.cbuf.((i + state.pos) mod state.length).(j)
        done
    done;
    !result

let rec loop1 data state i =
    if is_sum state data.(i) then begin
        update data state i;
        loop1 data state (i + 1)
    end else 
        data.(i)

let rec sum data offset length = if length = 0 then 0 else data.(offset) + sum data (offset + 1) (length - 1)

let rec loop2 data target offset length =
    let s = sum data offset length in
    if s = target then
        let slice = Array.sub data offset length in
        Array.fold_left max min_int slice + Array.fold_left min max_int slice
    else if s < target then
        loop2 data target offset (length + 1)
    else
        loop2 data target (offset + 1) (length - 1)

let _ =
    let data = "day9.input" |> Scanf.Scanning.open_in |> Seq.unfold read_line |> Array.of_seq in
    let state = init 25 data in 
    let key = loop1 data state 25 in
    Printf.printf "%d\n" key;
    loop2 data key 0 2 |> Printf.printf "%d\n"