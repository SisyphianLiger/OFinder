open Base
open Lib.Ls

let%test "rev" =
    List.equal Int.equal (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ];;

let%test "2D Matrices are the Same" = 
    let std_matrix = Array.make_matrix ~dimx:4 ~dimy:4 0 in
    let my_matrix = make_str_matrix 4 4 in
    Array.equal (Array.equal Int.equal) std_matrix my_matrix

let%test "Length of String is Accurate" = 
    let x = "Hello World" in
    Int.equal (str_len x) 11;;
