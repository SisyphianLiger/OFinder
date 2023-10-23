(* Generates a 2D Array *)

let make_str_matrix x_str_len y_str_len = 
    Array.make_matrix x_str_len y_str_len 0

(* To perform a pring Check on Matrixes *)
let print_matrix m = 
    Array.iter (fun row -> Array.iter
               (fun elem -> print_int elem; print_string " ") row;
                print_newline ()) m

(* Getting the str length for the Algorithm *)
let str_len str = String.length str
