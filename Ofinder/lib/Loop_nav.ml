(* Take in Input 
    From Ocaml.org docs 
    While loops are useful with references
    If we use a ref here that being false, 
    we are saying while the loop is not true 
    print_string q to quit and if we see that 
    the string.[0] is true then we set file_loop to true 
    setting the boolean value to the reference file_loop 
    to true terminating the loop

 *)
let string_test = "/test/string/input/here/match/"

let start_file_loop () = 
    let file_loop = ref false in 
    print_endline "";
    while not !file_loop do 
        print_string string_test; print_endline "";
        let str = read_line () in 
        match Some(str) with 
        | Some("q") -> file_loop := true 
        | Some(_) -> file_loop := false
        | _ -> file_loop := false
    done;

