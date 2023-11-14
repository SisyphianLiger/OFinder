open Curses
(* Take in Input 
   u
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

    I have a sentence and I want to ask more questions.


 *)

(* attron (A.color_pair 1); *)
(* attroff (A.color_pair 1); *)
let create_color_window () = 
    let mainwindow = initscr () in
    
    (* Needed to read text contiguously *)
    let _ = cbreak () in
    let _ = noecho () in  

    let (maxy, maxx) = getmaxyx mainwindow in
    (* Window Trial *)
    (* let win = newwin 25 50 25 50 in  *)
    let win_top = newwin (maxy * 7 / 10) maxx 2 0 in
    let win_bot = newwin (maxy / 10)  maxx (maxy * 9 / 10) 0  in
    box win_top (Char.code '|') (Char.code '=');
    box win_bot (Char.code '|') (Char.code '=');
    let _ = refresh () in

    let _ = mvaddstr 1 2 "FILES OR DIRECTORIES" in
    (* let _ = mvaddstr 3 2 "Directory/File Input" in *)
    let _ = mvaddstr ((maxy * 9 / 10) - 1) 2 "SEARCH TERM BOX:" in
    let _ = mvaddstr ((maxy * 9 / 10) + 1) 3 "" in

    let _ = wrefresh win_top in 
    let _ = wrefresh win_bot in 

    let backspace = 127 in 
    let escape = 27 in

    let ls_loop = ref true in
    let s_input = ref "" in
    while !ls_loop do 
        let key = getch () in 
        match key with 
        | key when key = backspace  ->  if String.length !s_input > 0 
                                        then 
                                            s_input := String.sub !s_input 0 (String.length !s_input - 1);
                                            ignore(mvaddstr ((maxy * 9 / 10) + 1) 2 (String.make maxx ' '));
                                            ignore(mvaddstr ((maxy * 9 / 10) + 1) 3 !s_input);
        | key when key = escape     ->  ls_loop := false

        | _ ->   s_input := !s_input  ^ Char.escaped (Char.chr key);            
                 ignore(mvaddstr ((maxy * 9 / 10) + 1) 3 !s_input);
    done;
    endwin ()
