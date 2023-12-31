open Base
open Lib.Ls
(* open Lib.Match_str *)

let%test "Testing LS Distance sitting/kitten OUTOFBOUNDS??" = 
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
    Int.equal m_cmp.((String.length str_b) - 1).((String.length str_a) - 1) m


let%test "Testing LS Distance Saturday/Sunday NEWTEST" = 
    let str_a = "Sunday" in
    let str_b = "Saturday" in
    let m = make_str_matrix str_a str_b in 
    let m_cmp = [|  [|0; 1; 2; 3; 4; 5; 6; 7; 8|]; 
                    [|1; 0; 1; 2; 3; 4; 5; 6; 7|];
                    [|2; 1; 1; 2; 2; 3; 4; 5; 6|];
                    [|3; 2; 2; 2; 3; 3; 4; 5; 6|];
                    [|4; 3; 3; 3; 3; 4; 3; 4; 5|];
                    [|5; 4; 3; 4; 4; 4; 4; 3; 4|];
                    [|6; 5; 4; 4; 5; 5; 5; 4; 3|];
                    |] in
    Int.equal m_cmp.(String.length str_a - 1).(String.length str_b - 1) m


(* let%test "Testing Match String match" =  *)
(*     let str_a = "Sunday" in *)
(*     let str_b = "Sun" in *)
(*     let res = my_match_str str_a str_b in  *)
(*     let find_res res =  *)
(*         match res with *)
(*         | Some(r) -> r *)
(*         | None -> "" *)
(*     in *)
(*     let m = "||Sun||day" in  *)
(*     String.equal m (find_res res) *)
(**)
(**)
(* let%test "Testing Match String match" =  *)
(*     let str_a = "Sunday" in *)
(*     let str_b = "day" in *)
(*     let res = my_match_str str_a str_b in  *)
(*     let find_res res =  *)
(*         match res with *)
(*         | Some(r) -> r *)
(*         | None -> "" *)
(*     in *)
(*     let m = "Sun||day||" in  *)
(*     String.equal m (find_res res) *)
(**)
(**)
(* let%test "Testing No Match String" =  *)
(*     let str_a = "Sunday" in *)
(*     let str_b = "lol" in *)
(*     let res = my_match_str str_a str_b in  *)
(*     let find_res res =  *)
(*         match res with *)
(*         | Some(r) -> r *)
(*         | None -> "" *)
(*     in *)
(*     let m = "" in  *)
(*     String.equal m (find_res res) *)
(**)
