open Curses
(* Take in Input 
    From Ocaml.org docs 
    While loops are useful with references
    If we use a ref here that being false, 
    we are saying while the loop is not true 
    print_string q to quit and if we see that 
    the string.[0] is true then we set file_loop to true 
    setting the boolean value to the reference file_loop 
    to true terminating the loop

    Initialize an empty search string before the loop starts.
    In your loop, take each character input from read_line ()
    and append it to your search string.
    Apply your algorithms to the search string inside the loop.
    If you do this, every time a new character is inputted, 
    the search string would get longer by one character, 
    and the algorithms would be applied to an increasingly 
    longer string, just as you intended.


 *)


let test_curses () =
    let win = initscr () in
    let _ = waddstr win "Hello, curses!" in
    let _ = wrefresh win in
    Unix.sleep 2;  (* Pause for 2 seconds *)
    endwin ()

let update s ns = s ^ ns

let start_file_loop () = 
    (* Initialize curses *)
    let win = initscr () in

    (* Initialize keypad listening *)
    let _ = keypad win true in

    (* Disable echo of input characters *)
    let _ = noecho () in  

    (* Initialize s_input and while loop*)
    let s_input = ref "" in
    let file_loop = ref false in 
    let key_backspace = 127 in 
    let key_escape = 27 in

    while not !file_loop do 
        let key = getch () in 
        match key with 
        | key when key = key_backspace -> if String.length !s_input > 0 then
                            s_input := String.sub !s_input 0 (String.length !s_input - 1)
        | key when key = key_escape -> file_loop := true

        | _ ->  s_input := !s_input  ^ Char.escaped (Char.chr key);
    done;

    let _ = waddstr win !s_input in
    let _ = wrefresh win in

    endwin ()

