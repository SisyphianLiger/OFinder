(* Levenshtein Algorithm *)

(* Find Min *)
let find_min x y z = 
    Int.min x (Int.min y z)

    (* Applies the function and returns min *)
let lsd_calc_min i j =
    let top = Int.max (i - 1) j in
    let mid = Int.max i (j - 1) in
    let bot = Int.max (i - 1) (j - 1) in 
    find_min top mid bot

    (* Checks if max or min path *)
let lsd_calc str_a str_b i j = 
    let str_chk = str_a = str_b in
    match str_chk with
        | true -> 0
        | false -> lsd_calc_min i j

(* Getting the str length for the Algorithm *)
let str_len str = String.length str

(* Generates a 2D Array *)
let make_str_matrix x_str y_str = 
    let x_str_len = str_len x_str in 
    let y_str_len = str_len y_str in
    Array.init x_str_len (fun y -> 
        Array.init y_str_len (
            fun x -> lsd_calc x_str.[y] y_str.[x] x y ))

(* To perform a pring Check on Matrixes *)

let print_matrix m = 
    Array.iter (fun row -> Array.iter
               (fun elem -> print_int elem; print_string " ") row;
                print_newline ()) m






