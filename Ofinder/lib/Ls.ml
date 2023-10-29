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
    let x_ss = (String.sub x_str 0 i) in
    let y_ss = (String.sub y_str 0 j) in
    let cmp = String.compare x_ss y_ss in
    let left = m.(i).(j-1) + 1 in
    let top = m.(i+1).(j) + 1 in 
    let cur = m.(i-1).(j-1) + if cmp = 0 then 0 else 1 in
    find_min left top cur
 



(* Generates a 2D Array: Functional *)
let make_str_matrix x_str y_str = 
    let x_str_len = str_len ("" ^  x_str) in 
    let y_str_len = str_len ("" ^ y_str) in
    let m = Array.init x_str_len (fun i -> 
        if i = 0 then Array.init y_str_len (fun j -> j + i)
        else Array.init y_str_len 
            (fun j -> if j = 0 
                then j + i 
                else 0)) in  
    m



(* Generating Ordered Matrix size of strs a and b: Imperative*)
(* let ordered_matrix str_a str_b =  *)
(*     let len_a = String.length str_a and len_b = String.length str_b in *)
(*     let matrix = Array.make_matrix len_a len_b 0 in  *)
(*     for i = 0 to len_a - 1 do  *)
(*         for j = 0 to len_b - 1 do  *)
(*             matrix.(i).(j) <- lsd_calc str_a.[i] str_b.[j] (i + 1) (j + 1) *)
(*         done *)
(*     done; *)
(*     matrix *)
