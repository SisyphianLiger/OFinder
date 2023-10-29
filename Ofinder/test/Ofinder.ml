open Base
open Lib.Ls

(* Testing Lib.ls *)
(* 2D Matrix Implementation Works *)
(* let%test "2D Matrices are the Same" =  *)
(*     let std_matrix = Array.make_matrix ~dimx:4 ~dimy:4 0 in *)
(*     let my_matrix = make_str_matrix "llll" "llll" in *)
(*     Array.equal (Array.equal Int.equal) std_matrix my_matrix *)
(**)
(* (* Length of String Works *) *)
(* let%test "Length of String is Accurate" =  *)
(*     let x = "Hello World" in *)
(*     Int.equal (str_len x) 11;; *)
(**)
(* (* Matrix Made with str_len *) *)
(* let%test "Testing that str_len and make_str_matrix work" =  *)
(*     let std_matrix = Array.make_matrix ~dimx:3 ~dimy:3 0 in *)
(*     (* let () =  print_matrix std_matrix in *) *)
(*     let my_matrix = make_str_matrix "lll" "lll" in  *)
(*     (* let () =  print_matrix my_matrix in *) *)
(*     Array.equal (Array.equal Int.equal) std_matrix my_matrix *)
(**)
(* (* Testing Min Function *) *)
(* let%test "Testing that min of 3 numbers works" =  *)
(*     Int.equal (find_min 1 2 3) 1 *)
(**)
(* (* Trying to draw out my matrix *) *)
(**)
(* let%test "2D Matrix is initialized" = *)
(*     let m = make_str_matrix "lll" "lll" in *)
(*     (* let () = print_matrix m in *) *)
(*     Int.equal m.(2).(2) 0;; *)
(**)

(* Test from example on webpage *)
let%test "Testing LS Distance" = 
    let str_a = "sitting" in
    let str_b = "kitten" in
    let m = make_str_matrix str_a str_b in 
    let m_cmp = [|  [|0; 1; 2; 3; 4; 5; 6|]; 
                    [|1; 1; 2; 3; 4; 5; 6|];
                    [|2; 2; 1; 2; 3; 4; 5|];
                    [|3; 3; 2; 1; 2; 3; 4|];
                    [|4; 4; 3; 2; 1; 2; 3|];
                    [|5; 5; 4; 3; 2; 2; 3|];
                    [|6; 6; 5; 4; 3; 3; 2|];
                    [|7; 7; 6; 5; 4; 4; 3|];
                |] in
    let () = print_matrix m in
    let () = Stdio.print_endline "" in
    let () = print_matrix  m_cmp in
    Array.equal (Array.equal Int.equal) m m_cmp

