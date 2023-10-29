(* To perform a pring Check on Matrixes *)
let print_matrix m = 
    Array.iter (fun row -> Array.iter
               (fun elem -> print_int elem; print_string " ") row;
                print_newline ()) m



                (* Levenshtein Algorithm *)

(* Getting the str length for the Algorithm *)
let str_len str = String.length str



    (* Find Min *)
let find_min x y z = 
    Int.min x (Int.min y z)


let lsd_calc_min m i j x_str y_str =
    (* let x_ss = (String.sub x_str 0 i) in *)
    (* let y_ss = (String.sub y_str 0 j) in *)
    (* let cmp = String.compare x_ss y_ss in *)
    let x_chr = String.get x_str (i-1) in
    let y_chr = String.get y_str (j-1) in
    let del = m.(i-1).(j) + 1 in
    let ins = m.(i).(j-1) + 1 in 
    let sub = m.(i-1).(j-1) + if x_chr = y_chr then 0 else 1 in
    find_min del ins sub 

    (* m.(i).(j) <- find_min left top cur;; *)
let rec map_calc_min m i j x_str y_str =
    if i <= String.length x_str  then
        if j <= String.length y_str  then
            begin
                m.(i).(j) <- lsd_calc_min m i j x_str y_str;
                map_calc_min m i (j + 1) x_str y_str;
            end
        else
            map_calc_min m (i + 1) 1 x_str y_str
    else 
        ()
        

(* Generates a 2D Array: Functional *)
let make_str_matrix x_str y_str = 
    let x_str_len = str_len (" " ^  x_str) in 
    let y_str_len = str_len (" " ^ y_str) in
    let m = Array.init x_str_len (fun i -> 
        if i = 0 then Array.init y_str_len (fun j -> j + i)
        else Array.init y_str_len 
            (fun j -> if j = 0 
        then j + i 
                else 0)) in  
    map_calc_min m 1 1 x_str y_str;
    m
